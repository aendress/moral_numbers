
#This doesn't work. 

bootstrap.weber.ratio <- function(dat = ., fit.a = FALSE, fit.w = TRUE, fit.params = FIT_PARAMS, w.chosen = NULL, within_subj = FALSE){
    
    if(within_subj){
        dat_samples <- dat %>% 
            dplyr::slice_sample(prop = 1, replace = TRUE) %>% 
            dplyr::group_by(n.saved, n.victims) %>% 
            dplyr::summarize(rating.bin = mean(rating.bin),
                             .groups = "drop")
            
    } else {
        dat_samples <- dat %>% 
            dplyr::group_by(ResponseId) %>% 
            dplyr::group_nest() %>% 
            dplyr::slice_sample(prop = 1, replace = TRUE) %>% 
            tidyr::unnest(cols = c(data)) %>% 
            dplyr::group_by(n.saved, n.victims) %>% 
            dplyr::summarize(rating.bin = mean(rating.bin),
                             .groups = "drop")
    }
    
    
    if ((!fit.a) & (fit.w)){
        # Fit w only
        tryCatch(
            #nls(
            minpack.lm::nlsLM(   
                rating.bin ~ acceptability.fnc (w, 
                                                n.saved, n.victims),
                data = dat_samples,
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
                data = dat_samples,
                start = FIT_PARAMS$start,
                #nlsLM specific options
                lower = FIT_PARAMS$lower,
                upper = FIT_PARAMS$upper) %>%
                coef() %>%
                t() %>%
                data.frame,
            error = function(e) data.frame (w = NA, a = NA))
        
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
                data = dat_samples,
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




dat.moral.numbers.bootstrap.1param.within.subj <- #furrr::future_imap_dfr(1:(N_BOOTSTRAP/500),
    furrr::future_imap_dfr(1:(500),
                                                                         ~ dat.moral.numbers.for.bootstrap.fit %>% 
                               dplyr::filter(experimentID == "exp1") %>% 
                                                                             dplyr::group_by(experimentID, blocks, decisionType, vivacity, ResponseId) %>% 
                                                                             dplyr::group_modify(~ bootstrap.weber.ratio(.x, fit.a = FALSE, fit.w = TRUE, within_subj = TRUE)),
                                                                         .options = furrr_options(seed = TRUE,
                                                                                                  stdout = !RECALCULATE_EVERYTHING))
#toc()

dat.moral.numbers.bootstrap.1param.within.subj.summary <- 
    Reduce(function(x, y){
        
        dplyr::left_join(x, y,
                         by = c("experimentID", "blocks", "decisionType", "vivacity"),
                         suffix = c("", "_prior"))
    },
    
    list(dat.moral.numbers.bootstrap.1param.summary,
         dat.moral.numbers %>%
             dplyr::filter(question == "acceptability") %>%
             dplyr::group_by(experimentID, ResponseId, decisionType, vivacity) %>%
             dplyr::summarize(rating.raw = mean(rating.raw),
                              rating.bin = mean(rating.bin),
                              .groups = "drop") %>%
             dplyr::mutate(blocks = "all")
         ),
    
    init = dat.moral.numbers.bootstrap.1param.within.subj  %>%
        dplyr::group_by(experimentID, blocks, decisionType, vivacity, ResponseId) %>% 
        summarize(across(everything(), list(M = mean, SD = sd, SE = se)),
                  .groups = "drop")
    ) %>% 
    dplyr::mutate(weight_M = exp(-((w_M - w_M_prior) / w_SD_prior)^2)) %>% 
    dplyr::mutate(w_hat = w_M * weight_M + w_M_prior * (1 - weight_M))
#    dplyr::mutate(w_hat = (w_M / w_SD^2 + w_M_prior / w_SD_prior^2) / (1/ w_SD^2 + 1 / w_SD_prior^2))
    
dat.moral.numbers.bootstrap.1param.within.subj.summary %>% 
    rstatix::cor_test(w_M, w_hat, rating.raw)


dat.moral.numbers.bootstrap.1param.within.subj.summary$w_SD

