# ----- Model definitions -----

discriminability.fnc <- function(w, n1, n2, ratio = NULL, a = NULL){
    
    if (!is.null (ratio)) {
        
        n1 <- ratio 
        n2 <- rep (1, length (ratio))
    }
    
    if (any (n1 < n2)){
        stop ("n1 must be greater than n2.")
    }
    
    
    # a is the relative weight of n2 vs. n1
    if (!is.null (a)){
        if (min(a) > 1) {
            # Scale n2, but don't make it greater than n1
            
            n2 <- pmin (n1, n2 * a)
            
        } else {
            # Scale n1
            # Scaling n1 with 1/a is equivalent to scaling
            # n2 with a
            
            n1 <- n1 / a
        }
        
    }
    
    # This is the fit according to Pica et al. (2004) and Halberda et al. (2008). It gives the estimation uncertainty.
    # Their equation has a factor of .5 before erfc, but we need to multiply pnorm (..., lower.tail = FALSE) with 2 as it reflects only 1 tail.
    
    # Similarly from ?pnorm
    # erfc <- function(x) 2 * pnorm(x * sqrt(2), lower = FALSE
    
    # This is the non-simplified version            
    # .estimation.uncertainty <- 1 - (2 * .5) * pnorm(
    #     sqrt(2) * (n1 - n2) / 
    #         (sqrt (2) * w * sqrt (n1^2 + n2^2)),
    #     lower.tail = FALSE)
    # Canceling redundant factors
    .estimation.uncertainty <- 1 - pnorm(
        (n1 - n2) / 
            (w * sqrt (n1^2 + n2^2)),
        lower.tail = FALSE)
    
    
    return (.estimation.uncertainty)
}

acceptability.fnc <- function(w, n1, n2, ratio = NULL, a = NULL){
    
    .estimation.uncertainty <- discriminability.fnc (
        w = w, n1 = n1, n2 = n2, ratio = ratio, a = a)
    
    # Convert estimation uncertainty to surprisal
    .surprisal <- -log2(.estimation.uncertainty)
    
    # Return the complement of surprisal as a measure of 
    # Uncertainty
    1 - .surprisal
    
}



# ----- Fit helper functions -----

generate_bootstrap_samples <- function(dat = ., subj_col = ResponseId, n_samples = 1000, reindex_subj_col = FALSE){
    
    subj_col <- dplyr::enquo(subj_col)
    
    furrr::future_imap(1:n_samples,
                       ~ {
                           
                           dat_sampled <- dat %>% 
                               dplyr::group_by(!!subj_col) %>% 
                               dplyr::group_nest(keep = FALSE) %>% 
                               dplyr::slice_sample(prop = 1, replace = TRUE)
                           
                           if(reindex_subj_col){ 
                               
                               # Re-index subjects
                               dat_sampled <- dat_sampled %>% 
                                   dplyr::mutate(!!subj_col := str_c("randomId_", row_number()))
                               
                           }
                           
                           
                           dat_sampled %>% 
                               tidyr::unnest(cols = c(data))
                           
                           
                           
                       },
                       .options = furrr_options(seed = TRUE,
                                                stdout = !RECALCULATE_EVERYTHING))
}

# Boot library is annoying for grouped data
bootstrap.weber.ratio <- function(dat = ., fit.a = FALSE, fit.w = TRUE, fit.params = FIT_PARAMS, w.chosen = NULL){
    
    
    if ((!fit.a) & (fit.w)){
        # Fit w only
        tryCatch(
            #nls(
            minpack.lm::nlsLM(   
                rating.bin ~ acceptability.fnc (w, 
                                                n.saved, n.victims),
                data = dat %>% 
                    dplyr::group_by(ResponseId) %>% 
                    dplyr::group_nest() %>% 
                    dplyr::slice_sample(prop = 1, replace = TRUE) %>% 
                    tidyr::unnest(cols = c(data)) %>% 
                    dplyr::group_by(n.saved, n.victims) %>% 
                    dplyr::summarize(rating.bin = mean(rating.bin),
                                     .groups = "drop"),
                # This is a list
                start = FIT_PARAMS$start["w"],
                #nlsLM specific options
                # These are vectors
                lower = FIT_PARAMS$lower["w"],
                upper = FIT_PARAMS$upper["w"]) %>% 
                coef() %>% 
                t() %>% 
                data.frame,
            error = function(e) data.frame (w = NA))
        
    } else if ((fit.a) & (fit.w)){
        # Fit a and w
        tryCatch(
            #nls(
            minpack.lm::nlsLM(   
                rating.bin ~ acceptability.fnc (w,
                                                n.saved, n.victims,
                                                a = a),
                data = dat %>% 
                    dplyr::group_by(ResponseId) %>% 
                    dplyr::group_nest() %>% 
                    dplyr::slice_sample(prop = 1, replace = TRUE) %>% 
                    tidyr::unnest(cols = c(data)) %>% 
                    dplyr::group_by(n.saved, n.victims) %>% 
                    dplyr::summarize(rating.bin = mean(rating.bin),
                                     .groups = "drop"),
                start = FIT_PARAMS$start,
                #nlsLM specific options
                lower = FIT_PARAMS$lower,
                upper = FIT_PARAMS$upper) %>%
                coef() %>%
                t() %>%
                data.frame,
            error = function(e) data.frame(w = NA, a = NA))
        
    } else if ((fit.a) & (!fit.w)){
        # Fit a and set w to w.chosen
        
        if(is.null(w.chosen)){
            stop("w.chosen must be set if fit.a is TRUE and fit.w is FALSE.")
        }
        
        tryCatch(
            #nls(
            minpack.lm::nlsLM(                   
                rating.bin ~ acceptability.fnc (w = w.chosen,
                                                n.saved, n.victims,
                                                a = a),
                data = dat %>% 
                    dplyr::group_by(ResponseId) %>% 
                    dplyr::group_nest() %>% 
                    dplyr::slice_sample(prop = 1, replace = TRUE) %>% 
                    tidyr::unnest(cols = c(data)) %>% 
                    dplyr::group_by(n.saved, n.victims) %>% 
                    dplyr::summarize(rating.bin = mean(rating.bin),
                                     .groups = "drop"),
                # This is a list
                start = FIT_PARAMS$start["a"],
                #nlsLM specific options
                # These are vectors
                lower = FIT_PARAMS$lower["a"],
                upper = FIT_PARAMS$upper["a"]) %>% 
                coef() %>%
                t() %>%
                data.frame,
            error = function(e) data.frame (a = NA))
        
    } else {
        stop("Unkown parameter combination")
    }
}

# This version is using broom and is more elegant but slower
# Not updated to allow for settiing w to specific value
# Not updated to use FIT.PARAMS instead of start.w and start.a either
bootstrap.weber.ratio.new <- function(dat = ., fit.a = FALSE, start.w = .6, start.a = 1){
    
    
    if (!fit.a){
        tryCatch(
            #nls(
            minpack.lm::nlsLM(
                rating.bin ~ acceptability.fnc (w, 
                                                n.saved, n.victims),
                data = dat %>% 
                    dplyr::group_by(ResponseId) %>% 
                    dplyr::group_nest() %>% 
                    dplyr::slice_sample(prop = 1, replace = TRUE) %>% 
                    tidyr::unnest(cols = c(data)) %>% 
                    dplyr::group_by(n.saved, n.victims) %>% 
                    dplyr::summarize(rating.bin = mean(rating.bin),
                                     .groups = "drop"),
                start = list(w = start.w)) %>% 
                broom::tidy(),
            error = function(e) data.frame(estimate = NA))
    } else {
        tryCatch(
            #nls(
            minpack.lm::nlsLM(
                rating.bin ~ acceptability.fnc (w,
                                                n.saved, n.victims,
                                                a = a),
                data = dat %>% 
                    dplyr::group_by(ResponseId) %>% 
                    dplyr::group_nest() %>% 
                    dplyr::slice_sample(prop = 1, replace = TRUE) %>% 
                    tidyr::unnest(cols = c(data)) %>% 
                    dplyr::group_by(n.saved, n.victims) %>% 
                    dplyr::summarize(rating.bin = mean(rating.bin),
                                     .groups = "drop"),
                start = list(w = start.w, a = start.a)) %>%
                broom::tidy(),
            error = function(e) data.frame(estimate = NA))
    }
}


get.by.subj.weber.ratio <- function(subj, dat = ., fit.a = FALSE){
    if (!fit.a){
        tryCatch(
            dat %>% 
                dplyr::filter(ResponseId == subj) %>%
                #nls(
                minpack.lm::nlsLM(
                    rating.bin ~ acceptability.fnc (w, 
                                                    n.saved, 
                                                    n.victims),
                    data = ., 
                    # This is a list
                    start = FIT_PARAMS$start["w"],
                    #nlsLM specific options
                    # These are vectors
                    lower = FIT_PARAMS$lower["w"],
                    upper = FIT_PARAMS$upper["w"]) %>% 
                coef,
            error = function(e) NA)
    } else {
        tryCatch(
            dat %>% 
                dplyr::filter(ResponseId == subj) %>%
                #nls(
                minpack.lm::nlsLM(
                    rating.bin ~ acceptability.fnc (w, 
                                                    n.saved, 
                                                    n.victims, 
                                                    a = a),
                    data = ., 
                    start = FIT_PARAMS$start,
                    #nlsLM specific options
                    # These are vectors
                    lower = FIT_PARAMS$lower,
                    upper = FIT_PARAMS$upper) %>% 
                coef,
            error = function(e) NA)
    }
}

# Like get.by.subj.weber.ratio, but doesn't filter and returns data frames
get.weber.ratio <- function(dat = ., fit.a = FALSE, fit.w = TRUE, w.chosen = NULL){
    
    if((!fit.a) & (fit.w)){
        # Fit only w
        tryCatch(
            dat %>%
                #nls(
                minpack.lm::nlsLM(
                    rating.bin ~ acceptability.fnc (w,
                                                    n.saved,
                                                    n.victims),
                    data = .,
                    # This is a list
                    start = FIT_PARAMS$start["w"],
                    #nlsLM specific options
                    # These are vectors
                    lower = FIT_PARAMS$lower["w"],
                    upper = FIT_PARAMS$upper["w"]) %>%
                coef %>%
                t() %>%
                data.frame,
            error = function(e) data.frame(w = NA))
    } else if((fit.a) & (fit.w)){
        # Fit both a and w
        tryCatch(
            dat %>%
                #nls(
                minpack.lm::nlsLM(
                    rating.bin ~ acceptability.fnc (w,
                                                    n.saved,
                                                    n.victims,
                                                    a = a),
                    data = .,
                    # This is a list
                    start = FIT_PARAMS$start,
                    #nlsLM specific options
                    # These are vectors
                    lower = FIT_PARAMS$lower,
                    upper = FIT_PARAMS$upper) %>%
                coef %>%
                t() %>%
                data.frame,
            error = function(e) data.frame(w = NA, a = NA))
    } else if((fit.a) & (!fit.w)){
        # Set w to value to w.chosen
        
        if(is.null(w.chosen)){
            stop("w.chosen must be set if fit.a is TRUE and fit.w is FALSE.")
        }
        
        tryCatch(
            dat %>%
                #nls(
                minpack.lm::nlsLM(
                    rating.bin ~ acceptability.fnc (w = w.chosen,
                                                    n.saved,
                                                    n.victims,
                                                    a = a),
                    data = .,
                    # This is a list
                    start = FIT_PARAMS$start["a"],
                    #nlsLM specific options
                    # These are vectors
                    lower = FIT_PARAMS$lower["a"],
                    upper = FIT_PARAMS$upper["a"]) %>%
                coef %>%
                t() %>%
                data.frame,
            error = function(e) data.frame(a = NA))
        
    } else {
        stop("Unknown parameter configuration")
    }
}

get.weber.ratio.linearized <- function(dat = ., fit.a = FALSE, fit.w = TRUE, w.chosen = NULL){
    
    # DONT USE: qnorm(2^(rating.bin - 1) is ionfite fopr rating.bin = =1
    
    # We use the following approximations around 1 and $\alpha$, respectively (see \ref(app:log-normal-approximations))
    # \frac{R-1}{\sqrt{R^2 + 1}} \approx \frac{\ln(R)}{\sqrt{2}}
    # \frac{R-\alpha}{\sqrt{R^2 + \alpha^2}} \approx \frac{\ln(R)/\alpha}{\sqrt{2}}
    
    # This gives use the linear equations (where $\alpha$ may be 1)
    # \ln (R/\alpha) = 2 w Phi^{-1} (2^(rating.bin-1))
    # and thus \ln (R) = \ln(\alpha) + 2 w Phi^{-1} (2^(rating.bin-1))
    
    if(fit.w & !fit.a){
        
        fit <- tryCatch(
            lm(lhs ~ 0 + rhs,
               data = dat %>% 
                   dplyr::mutate(
                       lhs = log(n.saved / n.victims),
                       rhs = 2 * qnorm(2^(rating.bin - 1))
                   )) ,
            error = function(e) NULL)
        
    } else if(fit.w & fit.a) {
        
        fit <- tryCatch(
            lm(lhs ~ rhs,
               data = dat %>% 
                   dplyr::mutate(
                       lhs = log(n.saved / n.victims),
                       rhs = 2 * qnorm(2^(rating.bin - 1))
                   )) ,
            error = function(e) NULL)
        
    } else if(!fit.w & fit.a) {
        
        if(is.null(w.chosen)) stop("w.chosen cannot be null") 
        
        fit <- tryCatch(
            lm(lhs ~ 1,
               data = dat %>% 
                   dplyr::mutate(
                       # subtract out slope
                       lhs = log(n.saved / n.victims) - w.chosen * 2 * qnorm(2^(rating.bin - 1))
                   )) ,
            error = function(e) NULL)
        
    } else {
        stop("Unknown parameter combination")
    }
    
    
    if(is.null(fit)) return(data.frame())
    
    fit_summary <- summary(fit)
    
    dplyr::bind_cols(
        broom::tidy(fit) %>% 
            dplyr::mutate(
                term = dplyr::case_when(
                    str_detect(term, "Intercept") ~ "a", # actuallly ln(a), still needs to be exponentiated
                    term == "rhs" ~ "w",
                    TRUE ~ NA_character_),
                estimate = if_else(term == "a", exp(estimate), estimate)), 
        data.frame(
            residual.se = fit_summary$sigma,
            r.squared = fit_summary$r.squared
        )
    )
}

# Added option to return prediction error
get.weber.ratio.with.prediction.error <- function(dat = ., fit.a = FALSE, fit.w = TRUE, w.chosen = NULL, return.prediction.error = FALSE){
    
    my_fit <- NULL  
    
    if((!fit.a) & (fit.w)){
        # Fit only w
        my_fit <- tryCatch(
            dat %>% 
                #nls(
                minpack.lm::nlsLM(
                    rating.bin ~ acceptability.fnc (w, 
                                                    n.saved, 
                                                    n.victims),
                    data = ., 
                    # This is a list
                    start = FIT_PARAMS$start["w"],
                    #nlsLM specific options
                    # These are vectors
                    lower = FIT_PARAMS$lower["w"],
                    upper = FIT_PARAMS$upper["w"]),
            error = function(e) NULL)
    } else if((fit.a) & (fit.w)){
        # Fit both a and w
        my_fit <- tryCatch(
            dat %>% 
                #nls(
                minpack.lm::nlsLM(
                    rating.bin ~ acceptability.fnc (w, 
                                                    n.saved, 
                                                    n.victims, 
                                                    a = a),
                    data = ., 
                    # This is a list
                    start = FIT_PARAMS$start,
                    #nlsLM specific options
                    # These are vectors
                    lower = FIT_PARAMS$lower,
                    upper = FIT_PARAMS$upper),
            error = function(e) NULL)
    } else if((fit.a) & (!fit.w)){
        # Set w to value to w.chosen
        
        if(is.null(w.chosen)){
            stop("w.chosen must be set if fit.a is TRUE and fit.w is FALSE.")
        }
        
        my_fit <- tryCatch(
            dat %>% 
                #nls(
                minpack.lm::nlsLM(
                    rating.bin ~ acceptability.fnc (w = w.chosen, 
                                                    n.saved, 
                                                    n.victims, 
                                                    a = a),
                    data = ., 
                    # This is a list
                    start = FIT_PARAMS$start["a"],
                    #nlsLM specific options
                    # These are vectors
                    lower = FIT_PARAMS$lower["a"],
                    upper = FIT_PARAMS$upper["a"]),
            error = function(e) NULL)
        
    } else {
        stop("Unknown parameter configuration")
    }
    
    
    # Turn results into data frame
    if(!is.null(my_fit)){
        df_fit <- my_fit  %>% 
            coef %>% 
            t() %>% 
            data.frame
    } else {
        output_params <- c("w", "a")[c(fit.w, fit.a)]
        
        df_fit <- matrix(NA_real_,
                         nrow = 1,
                         ncol = length(output_params),
                         dimnames = list(NULL, output_params)) %>% 
            as.data.frame()
    }
    
    if (return.prediction.error) {
        if(!is.null(my_fit)){
            preds <- ifelse(predict(my_fit, newdata = dat) > 0.5, 1, 0)
            df_fit$prediction_error <- mean(preds != dat$rating.bin)
        } else {
            df_fit$prediction_error <- NA
        }
    }
    
    return(df_fit)
    
}




# Fit a simple threshold model suggested by Josh
get.threshold <- function(dat = ., n.search = 1000) {
    
    # Compute the predictor variable
    dat <- dat %>%
        dplyr::mutate(ratio = n.saved / n.victims)
    
    # Define a fine grid of threshold values
    thresholds <- seq(min(dat$ratio), max(dat$ratio), length.out = n.search)
    
    # Evaluate prediction error for each threshold
    threshold_results <- furrr::future_map_dfr(thresholds, function(thresh) {
        preds <- ifelse(dat$ratio > thresh, 1, 0)
        error_rate <- mean(preds != dat$rating.bin)
        
        tibble(
            threshold = thresh,
            prediction_error = error_rate
        )
    })
    
    # Extract best threshold (lowest error)
    best_row <- threshold_results %>% 
        dplyr::filter(prediction_error == min(prediction_error)) %>% 
        dplyr::slice(1)
    
    # Return both best threshold and full result
    return(best_row)
}

get.estimates.from.bootstrap.fit <- function(dat = ., ...){

    d <- dat %>%
        dplyr::ungroup() %>%
        dplyr::filter(...) %>%
        dplyr::select(w_M, w_pi.lower, w_pi.upper) %>%
        dplyr::mutate(dplyr::across(dplyr::everything(), ~ round(.x, 3)))

    stringr::str_glue("$w$ = {d$w_M}, 95% PI [{d$w_pi.lower}, {d$w_pi.upper}]")
}

describe_fit_params <- function(params, which = c("start", "lower", "upper")) {
    which <- match.arg(which)
    values <- params[[which]]
    if (is.null(values)) {
        return("")
    }
    
    # Rename 'a' to Greek alpha in output
    names(values) <- ifelse(names(values) == "a", "\\alpha", names(values))
    
    paste0(
        sapply(seq_along(values), function(i) {
            sprintf("%.3g for \\(%s\\)", values[i], names(values)[i])
        }),
        collapse = " and "
    )
}
