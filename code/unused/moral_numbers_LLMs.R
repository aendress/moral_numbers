## ----extract-code, eval = FALSE---------------------------------------------------------------
# # Extract code to run on the command line, which is much faster than in the GUI
# 
# knitr::purl("chatGPT_test.Rmd", output = "chatGPT_test.R")
# 
# lintr::lint("chatGPT_test.R")


## ----setup, echo = FALSE, include=FALSE-------------------------------------------------------
rm (list=ls())


## ----load-libraries, include = FALSE, message = TRUE, warning = TRUE--------------------------

if (Sys.info()[["user"]] %in% c("ansgar", "endress")){
    source ("/Users/endress/R.ansgar/ansgarlib/R/tt.R")
    source ("/Users/endress/R.ansgar/ansgarlib/R/null.R")
    #source ("helper_functions.R")
} else {
    # Note that these will probably not be the latest versions
    source("http://endress.org/progs/tt.R")
    source("http://endress.org/progs/null.R")
}




librarian::shelf(
    tidyverse,
    stringr,
    knitr,
    kableExtra,
    ez,
    rlang,
    ggpubr,
    httr,
    jsonlite,
    #xml2,
    XML,
    glue,
   future,    
    furrr,
   ggthemes
#    xlsx,
#    purrr,
#    Hmisc,
#    stringi,
#    stringdist,
#    pwr
# broom,
#broom.mixed,
)

future::plan(multisession, workers = future::availableCores() - 1)
# #future::plan(multicore, workers = future::availableCores() - 1)
set.seed(1207100)



## ----combine-scenarios-numbers-define-fnc-----------------------------------------------------

# 1. Generate Latin Square indices
generate_latin_square <- function(n = 12, seed = NULL, return_matrix = FALSE) {
    
  if (!is.null(seed)) set.seed(seed)
    
    dat_lsd <- agricolae::design.lsd(1:n)$sketch %>%
        as.data.frame() %>%
        mutate(across(everything(), as.integer))
    
    if(isTRUE(return_matrix))
        dat_lsd <- as.matrix(dat_lsd)
    
    dat_lsd
}


apply_latin_design <- function(dat1, dat2, dat_lsd) {
    
    if(is.list(dat_lsd)){
        # if a list is provided, we need to extact the correct data frame 
        # based on the number of trials
        
        n_trials <- dat1 %>% nrow()
        
        dat_lsd <- dat_lsd[[paste0("n", n_trials)]]
    }
    
    
    map(seq_len(ncol(dat_lsd)), 
        function(order_idx) {
            cond_order <- dat_lsd[, order_idx]
            bind_cols(
                dat1,
                dat2[cond_order, ] %>%
                    mutate(lsd_order = order_idx)
            )
        })
    
}

combine_scenarios_and_number_conds <- function(expID){
    
    # Assumes that dat.scenario.content and dat.number.conds are global
    
    # Fix experimentID for exp2 (where we have exp2b and exp2ac, but the scenarios are the same)
    expID_scenarios <- ifelse(str_detect(expID, "exp2"),
                              "exp2",
                              expID)
    
    # Check if we have multiple conditions in an experiment
    # If yes, split the data frame, if not, put it into a list anyhow
    l_scenarios <- dat.scenario.content %>% 
        dplyr::filter(experimentID == expID_scenarios) %>%
        # Make experiment ID consistent
        dplyr::mutate(experimentID = expID)
    
    if(l_scenarios$condition %>% is.na %>% any){
        l_scenarios <- list(onlyCond = l_scenarios)
    } else {
        l_scenarios <- l_scenarios %>% 
            split(.$condition)
    }
    
    # Check if we have multiple groups for the number conditions 
    # If yes, split the data frame, if not, put it into a list anyhow
    l_number_conds <- dat.number.conds %>% 
        dplyr::filter(experimentID == expID)
    
    if(l_number_conds$Group %>% is.na %>% any){
        l_number_conds <- list(`Group 0` = l_number_conds)
    } else {
        l_number_conds <- l_number_conds %>% 
            split(.$Group)
    }
    
    # Extract names for combos
    dat_combo_names <- tidyr::expand_grid(
        experimentID = expID,
        scenarios = names(l_scenarios),
        number_conds = names(l_number_conds)
    ) %>% 
        tidyr::unite(combo_name, experimentID, scenarios, number_conds, sep = "_") %>% 
        dplyr:::mutate(combo_name = combo_name %>% 
                           str_remove_all(" ") %>% 
                           str_to_lower())
    
    expand_grid(
        scenarios = l_scenarios,
        number_conds = l_number_conds
    ) %>% 
        dplyr::bind_cols(dat_combo_names)
    
}

#' Combine Multiple Duplicate Columns into One (with Conflict Checking)
#'
#' This function identifies all columns in a data frame that match a given base name (e.g., "experimentID"),
#' checks that their non-NA values are consistent row-wise, and collapses them into a single column.
#' If conflicting non-NA values are found within a row, the function throws an error.
#' Redundant copies (e.g., `experimentID...2`, `experimentID...8`) are removed.
#'
#' @param dat A data frame or tibble. Defaults to the current data pipe (`.`).
#' @param col_name A string indicating the base column name to combine (e.g., `"experimentID"`).
#'
#' @return A data frame with a single resolved `col_name` column and redundant matching columns removed.
#' @export
#'
#' @examples
#' df <- tibble::tibble(
#'   experimentID = c("exp1", "exp2", NA),
#'   `experimentID...2` = c("exp1", "exp2", "exp3")
#' )
#' 
#' combine_multiple_copies_of_columns(df, "experimentID")
#'
combine_multiple_copies_of_columns <- function(dat = ., col_name = "experimentID"){
    
    dat %>% 
        dplyr::rowwise() %>% 
        dplyr::mutate(!!sym(col_name) := {
            vals <- dplyr::c_across(dplyr::matches(paste0("^", col_name))) %>% na.omit() %>% unique()
            if (length(vals) > 1) {
                stop(glue::glue("Mismatched {col_name} values: {paste(vals, collapse = ', ')}"))
            }
            vals[1] 
        }
        ) %>% 
        dplyr::select(-dplyr::matches(paste0(col_name, ".+")))
}  



## ----xml-define-function----------------------------------------------------------------------

xml_to_df <- function(xml_data = ., target_node_name = "scenario"){
    
    #top_name <- xml2::xml_name(xml2::xml_root(xml_data))
    
    # Find all target nodes (e.g., <scenario>)
    target_nodes <- xml2::xml_find_all(xml_data, paste0(".//", target_node_name))
    
    # Get child node names from the first target node
    target_sub_node_names <- purrr::map(target_nodes, 
                         ~ {
                            xml2::xml_children(.x) %>% xml2::xml_name() 
                         })[[1]]

    # Build a data frame: one row per node
    purrr::map_dfr(target_nodes,
                   # for each target node (e.g., scenario)
                   function(n){
                       # Loop through the names of the subnotes
                       purrr::map(target_sub_node_names,
                                  
                                  ~ tibble::tibble(!!rlang::sym(.x) := 
                                               xml2::xml_find_first(n, paste0("./", .x)) %>% 
                                               xml2::xml_text())) %>% 
                           # Combine the one colum tibbles into a single row
                           purrr::list_cbind()
                   })
    
}



## ----helper-functions-model-definition-copied-------------------------------------------------
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






## ----define-parameters-old--------------------------------------------------------------------

EXPERIMENT_DIR <- "../experiments/"



## ----define-parameters-new--------------------------------------------------------------------

# Load stuff from the main analysis script
OUTPUT_DIR <- "output/"
load(file.path(OUTPUT_DIR, "tmp_stuff_for_llm.RData"))

# Load API keys from .Renviron (not .profile)
list("OPENAI_API_KEY",
     "GEMINI_API_KEY",
     "ANTHROPIC_API_KEY",
     "DEEPSEEK_API_KEY",
     "MISTRAL_API_KEY",
     "QWEN_API_KEY"
     ) %>% 
    purrr::walk(~ assign(.x, Sys.getenv(.x), envir = .GlobalEnv))

# Secondary tier for experiments.ade@gmail.com, still at free tier
#@GEMINI_API_KEY <- Sys.getenv("GEMINI_API_KEY_SECONDARY")


# System prompts
SYSTEM_PROMPTS <- list(
    estimation = "When asked about the acceptability of an action, respond on a 6-point scale (1 = completely unacceptable, 6 = completely acceptable).",
    comparison = "When asked about the relative acceptability of two actions, respond on a 6-point scale (1 = Option 1 is much more unacceptable, 6 = Option 2 is much more acceptable)."
) %>% 
    purrr::map(~ glue("You are an average adult with no formal training in philosophy, ethics, or law. Your sense of right and wrong comes from everyday life—social norms, gut feelings, personal experience. You don’t try to be logically consistent or follow abstract moral rules. You react like a regular person would: practically, emotionally, and socially. You speak plainly, without academic or philosophical jargon. You can hesitate, contradict yourself, or be unsure—just like people often are. Please give your gut-level reaction, even if you’re not sure. 
      
     {.x}
      
    Respond **only** with a single digit (1–6). Do **not** add words, punctuation, or explanations. Any other output but the number will make your response unusable."))



# For gemini, the system prompt is part of the API call, not the message
SYSTEM_INSTRUCTIONS <- SYSTEM_PROMPTS %>% 
    purrr::map(~ list(
        parts = list(
            list(text = .x)
        )
    )
    )

LLM_OPTIONS <- list(
    max_attempts = 10,
    max_output_tokens = 10, # max tokens to generate in response
    temperature = .1, # controls randomness (0 = deterministic)
    top_k = 40,
    top_p = 1,
    # openAI specific options 
    n = 1, # number of completions
    # Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim.
    frequency_penalty = 0,  # optional, penalizes frequent tokens
    # Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics.
    presence_penalty = 0,    # optional, penalizes new topic introduction
    # ollama specific options
    num_ctx = 8192  # 8k context window
)

MODELS <- list(
    cloud = c(
        #"gpt-4o-mini", # This seems to be completely deontological
        #"gpt-4o",
        #"gpt-4.1",
        #"gpt-4.1-mini",
        #"claude-sonnet-4-5-20250929",
        #"gemini-2.5-flash",
        #"gemini-2.5-pro",
        "mistral-medium-latest",
        "deepseek-chat",
        "qwen-plus",
        "gpt-5-mini"
        ),
    local = c("llama3.2",
              "mistral",
              "mistral-nemo")
)

LLM_URLS <- c(
    gpt = "https://api.openai.com/v1/chat/completions",
    claude = "https://api.anthropic.com/v1/messages",
    `gemini-2.5-flash` = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent",
    `gemini-2.5-pro` = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-pro:generateContent",
    ollama = "http://localhost:11434/api/chat",
    `mistral` = "https://api.mistral.ai/v1/chat/completions",
    `deepseek` = "https://api.deepseek.com/chat/completions",
    `qwen-plus` = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1/chat/completions"
    
)

# Due to our latin square design, we already have 12 (10 for exp 4) subjects
# We multiple this by the number below. for example, with N_SUBJ_LLM_AFTER_LSD = 10, we effectively have 120 or 100 subjects
N_SUBJ_LLM_AFTER_LSD <- 10


RECALCULATE_LLM <- list(local = FALSE,
                        cloud = TRUE)

CHECKPOINT_DIR <- "checkpoints"


## ----load-scenarios-copied--------------------------------------------------------------------

dat.scenario.files <- tibble::tribble(
    ~experimentID, ~condition, ~file,

    "exp1", NA, paste0(EXPERIMENT_DIR, "experiment1/scenarios.xml"),
    "exp1b", NA, paste0(EXPERIMENT_DIR, "experiment1b/scenarios_replication.xml"),
    "exp2", "moral", paste0(EXPERIMENT_DIR, "experiment2_3/final/scenarios_xml/Scenarios_Moral_Choice_Version.xml"),
    "exp2", "economic", paste0(EXPERIMENT_DIR, "experiment2_3/final/scenarios_xml/Scenarios_Economic_Choice_Version.xml"),
    "exp4", "economic", paste0(EXPERIMENT_DIR, "experiment4/scenarios_replication_economic.xml"),
    # Same as in exp1b
    "exp4", "moral", paste0(EXPERIMENT_DIR, "experiment1b/scenarios_replication.xml"),
    "exp8", NA, paste0(EXPERIMENT_DIR, "experiment8_incommensurate_scenarios/incommensurate_scenarios_single_horrificness_question.xml"),
    "exp9a", NA, paste0(EXPERIMENT_DIR, "experiment9_verify_asymptote/incommensurate_scenarios_single_horrificness_question.10.scenarios.xml"),
    "exp10", NA, paste0(EXPERIMENT_DIR,
                        "experiment10_incommensurate_scenarios_new_ratios/incommensurate_scenarios_single_horrificness_question.new_questions.scenarios.exp10.xml"),
    "exp11", NA, paste0(EXPERIMENT_DIR, "experiment11_incommensurate_scenarios_new_ratios_replication_of_exp10/incommensurate_scenarios_single_horrificness_question.new_questions.scenarios.exp10.replication.xml"))

    

# Other files for experiments 3, 5 and 6
    # "exp3x", "evil", "/Users/endress/Experiments/moral_numbers/experiment2_3/final/scenarios_xml/Scenarios_Evil_Choice_Version.xml",
    # "exp3y", "evil-outgroup", "/Users/endress/Experiments/moral_numbers/experiment2_3/final/scenarios_xml/Scenarios_Evil_Choice_Version_Ingroup_favoritism.xml",

# "/Users/endress/Experiments/moral_numbers/experiment5/scenarios_xml/scenarios_vivd_victims_neutral_version.xml",
# "/Users/endress/Experiments/moral_numbers/experiment5/scenarios_xml/scenarios_vivd_victims_vivid_version.xml",
# "/Users/endress/Experiments/moral_numbers/experiment5/scenarios_xml/scenarios_vivd_victims_vivid_version_with_bold_face.xml"

# "/Users/endress/Experiments/moral_numbers/experiment6/scenarios_xml/scenarios_vivd_victims_neutral_version_3rd_person.xml"
# "/Users/endress/Experiments/moral_numbers/experiment6/scenarios_xml/scenarios_vivd_victims_vivid_version_first_person_with_bold_face.xml"

dat.scenario.content <- dat.scenario.files %>% 
    dplyr::group_by(experimentID, condition) %>% 
    dplyr::group_modify(~  .x$file %>% 
                            xml2::read_xml(.) %>% 
                            XML::xmlParse() %>% 
                            XML::xmlToDataFrame()) %>% 
    dplyr::ungroup() %>% 
    tidyr::separate_wider_delim(label, names = c("label", "condition2"), delim = ".", too_few =  "align_start") %>% 
dplyr::mutate(condition = coalesce(condition, condition2)) %>% 
    dplyr::select(experimentID, condition, title, text, dplyr::starts_with("question"), starts_with("option")) %>%
    dplyr::select(-dplyr::matches("anchor")) %>% 
    dplyr::mutate(question.acceptability = dplyr::coalesce(question, question.acceptability)) %>% 
    dplyr::select(-question) %>% 
    dplyr::rename(question.severity = question.severity.text) %>% 
    tidyr::drop_na(title, text) %>% 
    dplyr::mutate(across(where(is.character), ~ .x %>%
                            #str_remove_all("\\n") %>%
                             str_remove_all("[\\t\\n]") %>% 
                             str_remove_all("\\[br\\]") %>% 
                             str_squish())) 





## ----load-number-conditions-copied------------------------------------------------------------

dat.number.conds <- 
    # Define paths and other information for number conditions
    tibble::tribble(
        ~relative_path, ~experimentID, ~experimentDesc, ~Group, ~rename_victims,
        
        "experiment1/number_conds.csv",
        "exp1",
        "Experiment 1a",
        NA,
        FALSE,
        
        "experiment1b/number_conds_replication.csv",
        "exp1b",
        "Experiment 1b",
        NA,
        FALSE,
        
        "experiment2_3/final/number_conditions/number_conds_2x2_choices_ratio_vs_utility.csv",
        "exp2ac",
        "Experiment 2a and 2c (replication): Pitting the ratio against the utility: ",
        NA,
        TRUE,
        
        "experiment2_3/final/number_conditions/number_conds_2x2_choices_ratio_vs_utility_1.5utility_3xvictims.csv",
        "exp2d",
        "Experiment 2d: Replication of Experiment 2a/2c with less extreme contrasts",
        NA,
        TRUE,
        
        "experiment2_3/final/number_conditions/number_conds_2x2_choices_ratio_vs_victims.csv",
        "exp2b",
        "Experiment 2b: Pitting the ratio against the number of victims",
        NA,
        TRUE,
        
        "experiment4/number_conds_replication.csv",
        "exp4",
        "Experiment 4",
        NA,
        FALSE,
        
        "experiment5/number_conds/number_conds_vivid_victims.csv",
        "exp5a",
        "Experiment 5a",
        NA,
        FALSE,
        
        "experiment5/number_conds/number_conds_vivid_victims_5b.csv",
        "exp5b",
        "Experiment 5b",
        NA,
        FALSE,
        
        "experiment6/number_conds/number_conds_vivid_victims_exp6.csv",
        "exp6",
        "Experiment 6",
        NA,
        FALSE,
        
        "experiment7/number_conds_exp7_counterbalancing_group1.csv",
        "exp7",
        "Experiment 7",
        "Group 1",
        FALSE,
        
        "experiment7/number_conds_exp7_counterbalancing_group2.csv",
        "exp7",
        "Experiment 7",
        "Group 2",
        FALSE,
        
        "experiment8_incommensurate_scenarios/number_conds_exp8_counterbalancing_group1.csv",
        "exp8",
        "Experiment 8",
        "Group 1",
        FALSE,
        
        "experiment8_incommensurate_scenarios/number_conds_exp8_counterbalancing_group2.csv",
        "exp8",
        "Experiment 8",
        "Group 2",
        FALSE,
        
        "experiment9_verify_asymptote/number_conds_exp9a_counterbalancing_group1.csv",
        "exp9a",
        "Experiment 9a",
        "Group 1",
        FALSE,
        
        "experiment9_verify_asymptote/number_conds_exp9a_counterbalancing_group2.csv",
        "exp9a",
        "Experiment 9a",
        "Group 2",
        FALSE,
        
        "experiment10_incommensurate_scenarios_new_ratios/number_conds_exp10_counterbalancing_group1.csv",
        "exp10",
        "Experiment 10",
        "Group 1",
        FALSE,
        
        "experiment10_incommensurate_scenarios_new_ratios/number_conds_exp10_counterbalancing_group2.csv",
        "exp10",
        "Experiment 10",
        "Group 2",
        FALSE,
        
        # This is a replicated of Exp 10
        "experiment11_incommensurate_scenarios_new_ratios_replication_of_exp10/number_conds_exp10_counterbalancing_group2.csv",
        "exp11",
        "Experiment 11",
        "Group 1",
        FALSE,
        
        "experiment11_incommensurate_scenarios_new_ratios_replication_of_exp10/number_conds_exp10_counterbalancing_group2.csv",
        "exp11",
        "Experiment 11",
        "Group 2",
        FALSE,
        
        
        
    ) %>% 
    # LOOP through the files using pmap_dfr
    purrr::pmap_dfr(function(relative_path, experimentID, experimentDesc, Group, rename_victims) {
        
        file_path <- file.path(EXPERIMENT_DIR, relative_path)
        
        df <- readr::read_csv(file_path, show_col_types = FALSE) %>%
            dplyr::mutate(
                experimentID = experimentID,
                experimentDesc = experimentDesc,
                .before = 1
            )
        
        if(rlang::has_name(df, "ratio_saved_victims"))
            df <- dplyr::mutate(df, ratio_saved_victims = as.character(ratio_saved_victims))
        
        if (!is.na(Group)) {
            df <- dplyr::mutate(df, Group = Group, .before = 1)
        }
        
        if (rename_victims) {
            df <- dplyr::rename_with(df, ~ stringr::str_remove(.x, "1$"), dplyr::ends_with("1"))
        }
        
        return(df)
    }
    ) %>% 
    dplyr::select(experimentID, experimentDesc, Group, starts_with("n_")) %>% 
    dplyr::mutate(ratio = n_saved / n_victims,
                      #get.number.ratio(n_saved / n_victims, FALSE),
                  .after = "n_total") %>% 
    dplyr::mutate(ratio2 = n_saved2 / n_victims2,
                      #get.number.ratio(n_saved2 / n_victims2, FALSE),
                  .after = "n_total2") 



## ----combine-scenarios-numbers----------------------------------------------------------------



# dat.scenario.content %>% 
#     dplyr::filter(str_detect(experimentID, "exp10"))

l_lsd <- list(n10 = generate_latin_square(10),
                n12 = generate_latin_square(12))


# exp2b: ratio against vitims
# exp2ac: raiot again utility
# exp4

# 2. Cross scenarios × condition groups
dat_scenarios_numbers <- purrr::map(
    c("exp2b", "exp2ac", "exp4", "exp11"),
    combine_scenarios_and_number_conds
)


dat_scenarios_numbers <- purrr::map(
    dat_scenarios_numbers,
    function(dat){
        combo_names <- dat$combo_name
        
        purrr::pmap(list(dat$scenarios, dat$number_conds),
                    ~ apply_latin_design(..1, ..2, dat_lsd = l_lsd)) %>% 
            purrr::set_names(combo_names)
    }
)

make_prompt <- function(text, question, n_saved, n_victims, n_saved2, n_victims2, option1, option2){
    
    
    prompt <- dplyr::if_else(is.na(n_saved2),
                             glue::glue("{text} {question}"),
                             glue::glue("{text} {question} (1) {option1} (2) {option2}")
    )
    
    prompt <- dplyr::if_else(is.na(n_saved2),
                             prompt %>% 
                                 str_replace_all("n_victims", as.character(n_victims)) %>% 
                                 str_replace_all("n_saved", as.character(n_saved)),
                             
                             prompt %>% 
                                 str_replace_all("n_victims2", as.character(n_victims2)) %>% 
                                 str_replace_all("n_saved2", as.character(n_saved2)) %>% 
                                 str_replace_all("n_victims1", as.character(n_victims)) %>% 
                                 str_replace_all("n_saved1", as.character(n_saved))
    )
    
    prompt <- prompt %>% str_squish()
    
    
    prompt
    
}
    
dat_scenarios_numbers_flat <- dat_scenarios_numbers %>% 
    purrr::map(function(l){
        purrr::imap_dfr(l, ~ 
                            purrr::list_rbind(.x), .id = "combo_id") %>% 
            dplyr::mutate(
                dplyr::across(starts_with("n_"), as.integer),
                prompt = make_prompt(text, question.acceptability, n_saved, n_victims, n_saved2, n_victims2, option1, option2)) %>% 
            combine_multiple_copies_of_columns 
    }) %>% 
    purrr::list_rbind() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(subj = list(subj = glue::glue("{combo_id}_lsd{lsd_order}_s{1:N_SUBJ_LLM_AFTER_LSD}") %>% 
                              as.character())) %>% 
    tidyr::unnest(subj) 
    




## ----api-define-fnc---------------------------------------------------------------------------


get_model_url <- function(model = .){
    
    model <- str_to_lower(model)
    
    if(str_detect(model, "gpt")) return(LLM_URLS["gpt"] %>% unname)
    
    if(str_detect(model, "claude")) return(LLM_URLS["claude"] %>% unname)
    
        # We have different versions of gemini with different urls    
    if(str_detect(model, "gemini")) return(LLM_URLS[model] %>% unname)
    
    if(str_detect(model, "mistral")) return(LLM_URLS["mistral"] %>% unname)
    
    if(str_detect(model, "deepseek")) return(LLM_URLS["deepseek"] %>% unname)
    
    if(str_detect(model, "qwen")) return(LLM_URLS["qwen-plus"] %>% unname)
    
    return(LLM_URLS["ollama"] %>% unname)
}

make_llm_header <- function(model) {
    
    model_lower <- str_to_lower(model)
    
    # Detect model provider and return appropriate headers
    if (str_detect(model_lower, "gpt")) {
        return(c(Authorization = paste("Bearer", OPENAI_API_KEY)))
        
    } else if (str_detect(model_lower, "claude")) {
        return(c(
            `x-api-key` = ANTHROPIC_API_KEY, 
            `anthropic-version` = "2023-06-01"
        ))
        
    } else if (str_detect(model_lower, "gemini")) {
        return(c(`x-goog-api-key` = GEMINI_API_KEY))
        
    } else if (str_detect(model_lower, "mistral")) {
        return(c(Authorization = paste("Bearer", MISTRAL_API_KEY)))
        
    } else if (str_detect(model_lower, "deepseek")) {
        return(c(Authorization = paste("Bearer", DEEPSEEK_API_KEY)))
        
    } else if (str_detect(model_lower, "qwen")) {
        return(c(Authorization = paste("Bearer", QWEN_API_KEY)))
        
    } else {
        return(NULL)
    }
}

make_llm_body <- function(model, messages, system_prompt = NULL, system_instruction = NULL) {
    model_lower <- str_to_lower(model)
    
    # Build body depending on model
    if (str_detect(model_lower, "gpt-4")) {
        return(list(
            model = model,
            messages = messages,
            max_tokens = LLM_OPTIONS$max_output_tokens,        # max tokens to generate in response
            temperature = LLM_OPTIONS$temperature,        # controls randomness (0 = deterministic)
            #top_p = LLM_OPTIONS$top_p,              # nucleus sampling (1 = no restriction)
            n = LLM_OPTIONS$n                  # number of completions to generate
            # top_k is not a direct parameter in OpenAI API (used internally)
            #frequency_penalty = 0,  # optional, penalizes frequent tokens
            #presence_penalty = 0    # optional, penalizes new topic introduction
        ))
        
    } else if (str_detect(model_lower, "gpt-5")) {
        return(list(
            model = model,
            messages = messages,
            #max_completion_tokens = LLM_OPTIONS$max_output_tokens,        # max tokens to generate in response
            # Not supported
            #temperature = LLM_OPTIONS$temperature,        # controls randomness (0 = deterministic)
            
            #top_p = LLM_OPTIONS$top_p,              # nucleus sampling (1 = no restriction)
            n = LLM_OPTIONS$n                  # number of completions to generate
            # top_k is not a direct parameter in OpenAI API (used internally)
            #frequency_penalty = 0,  # optional, penalizes frequent tokens
            #presence_penalty = 0    # optional, penalizes new topic introduction
        ))
        
    } else if (str_detect(model_lower, "claude")) {
        return(list(
            model = model,
            system = system_prompt,
            messages = messages,
            max_tokens = LLM_OPTIONS$max_output_tokens,        # max tokens to generate in response
            temperature = LLM_OPTIONS$temperature        # controls randomness (0 = deterministic)
            # can't set top_p togther with temperature
            #top_p = LLM_OPTIONS$top_p              # nucleus sampling (1 = no restriction)
            #top_k = LLM_OPTIONS$top_k, # claude supports top_k
        ))
        
    } else if (str_detect(model_lower, "gemini")) {
        return(list(
            #model = model, # spefified in url
            contents = messages, 
            systemInstruction = system_instruction,
            generationConfig = list(
                temperature = LLM_OPTIONS$temperature,
                topP = LLM_OPTIONS$top_p #,
                #topK = LLM_OPTIONS$top_k,
                #maxOutputTokens = LLM_OPTIONS$max_output_tokens
            )
        ))
        
    } else if (str_detect(model_lower, "mistral")) {
        #https://docs.mistral.ai/api/
        return(list(
            model = model,    
            messages = messages,
            temperature = LLM_OPTIONS$temperature,
            # Mistral recommends not setting top_p and temperature
            #top_p = LLM_OPTIONS$top_p,
            max_tokens = LLM_OPTIONS$max_output_tokens,
            #presence_penalty = 0,
            #frequency_penalty = 0,
            n = LLM_OPTIONS$n,
            stream = FALSE
        ))
        
    } else if (str_detect(model_lower, "deepseek")) {
        # https://api-docs.deepseek.com/
        return(list(
            model = model,
            messages = messages,
            temperature = LLM_OPTIONS$temperature,
            # Don't set together with temperature 
            #topP = LLM_OPTIONS$top_p,
            #frequencyPenalty = 0, # Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim.
            #presencePenalty = 0, # Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics.
            maxTokens = LLM_OPTIONS$max_output_tokens,
            stream = FALSE
            # stream = TRUE,
            # streamOptions = list(
            #     includeUsage = TRUE,
            #     continuousUsageStats = TRUE
            # )
        ))
        
    } else if (str_detect(model_lower, "qwen")) {
        # https://qwen.ai/apiplatform
        return(list(
            model = model,
            messages = messages,
            temperature = LLM_OPTIONS$temperature,
            # Don't set together with temperature
            #top_p = LLM_OPTIONS$top_p,
            top_k = LLM_OPTIONS$top_k,
            #frequency_penalty = 0, # Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim.
            #presence_penalty = 0, # Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics.
            max_tokens = LLM_OPTIONS$max_output_tokens,
            n = LLM_OPTIONS$n,
            stream = FALSE
            # stream = TRUE,
            # streamOptions = list(
            #     includeUsage = TRUE,
            #     continuousUsageStats = TRUE
            # )
        ))
        
    } else {
        # Ollama or default
        return(list(
            model = model,
            messages = messages,
            options = list(
                temperature = LLM_OPTIONS$temperature,
                # top_k = LLM_OPTIONS$top_k,
                # top_p = LLM_OPTIONS$top_p,
                num_ctx = LLM_OPTIONS$num_ctx  # 8k context window
            ),
            stream = FALSE
        ))
    }
}

parse_llm_response <- function(model, response) {
    model_lower <- str_to_lower(model)
    # Parse response depending on model
    if (str_detect(model_lower, "gpt")) {
        result <- content(response)$choices %>% 
            purrr::map_chr(purrr::pluck, "message", "content")
            
    } else if (str_detect(model_lower, "claude")) {
        result <- purrr::pluck(content(response, as = "parsed"), "content", 1, "text")
        
    } else if (str_detect(model_lower, "gemini")) {
        result <- purrr::pluck(content(response, as = "parsed"), "candidates", 1, "content", "parts", 1, "text")
        
    } else if (str_detect(model_lower, "mistral|deepseek|qwen")) {
        result <- purrr::pluck(content(response, as = "parsed"), "choices", 1, "message", "content") %>% 
            str_trim()
            
    } else {
        result <- purrr::pluck(content(response, as = "parsed"), "message", "content")
    }
    
    return(result)
}

send_llm_request <- function(model, url, messages, system_prompt = SYSTEM_PROMPTS$estimation, system_instruction = SYSTEM_INSTRUCTIONS$estimation, DEBUG = FALSE, JUST_COUNT_CHARACTERS = FALSE){
    
    model_lower <- str_to_lower(model)
    
    if(isTRUE(JUST_COUNT_CHARACTERS)){
        
        if(str_detect(model, "gemini")){
            stop("We cannot do the character count for gemini messages.")
        }
        n_characters <- return(purrr::map_chr(messages,
                                   ~ pluck(.x, "content")) %>%
                                   str_length() %>%
                                   sum %>%
                                   as.character())
            
    }
    
     # Build headers depending on model (API)
    headers <- make_llm_header(model_lower)

    # Build body depending on model
    body_json <- make_llm_body(model_lower, messages, system_prompt, system_instruction)

        if(isTRUE(DEBUG)){
        cat("\n=== API Request Debug ===\n")
        print(body_json)
        cat("==========================\n")
    }

    # Now make the request
    attempt <- 1
    repeat{
        response <- httr::RETRY(
            verb = "POST",
            url = url,
            httr::add_headers(.headers = headers),
            content_type_json(),
            encode = "json",
            body = toJSON(body_json, auto_unbox = TRUE),
            times = LLM_OPTIONS$max_attempts,                  # retry up to 5 times
            pause_base = 30,
            pause_min = 10,               # minimum wait between retries (seconds)
            pause_cap = 60,              # maximum wait
            terminate_on = c(400, 401)   # don't retry on these status codes
        )
        
         status <- httr::status_code(response)

         # If 429, handle retry with backoff
         if (status == 429) {
             retry_after <- httr::headers(response)[["retry-after"]]
             wait_time <- if (!is.null(retry_after)) {
                 as.numeric(retry_after)
             } else {
                 #min(2^(attempt - 1) * 5, 60)
                 30
             }
             message(glue::glue("Rate limited (429). Sleeping for {wait_time} seconds (attempt {attempt}/{LLM_OPTIONS$max_attempts})..."))
             Sys.sleep(wait_time)
             attempt <- attempt + 1
             if (attempt > LLM_OPTIONS$max_attempts) {
                 stop("Max retry attempts reached due to rate limiting (429). Aborting.")
             }
             next  # retry loop
         }
         
         # For permanent errors, stop immediately
         if (status %in% c(400, 401)) {
             
             print(glue::glue("API returned status {status}, aborting."))
             print("\n Full response \n")
             #print(response)
             print(content(response, as = "text"))
             stop("Exiting")
         }
         
         # Break on success or other statuses
         break
    }
    

    if(isTRUE(DEBUG)){
        # 🔍 DEBUGGING PRINT
        cat("\n=== API Response Debug ===\n")
        cat("\n** Messages **\n")
        print(messages)
        cat("\n** Response **\n")
        print(response)
        cat("\nStatus Code:", status_code(response), "\n")
        cat("\nRaw Content:\n")
        print(content(response, as = "text"))
        cat("\nSystem Prompt:\n")
        print(system_prompt)
        cat("\nSystem instruction:\n")
        print(system_instruction)
        
        cat("==========================\n")
    }
    

    result <- parse_llm_response(model_lower, response)
    
    result
    
}


compose_llm_messages <- function(dat = ., model){
    # dat is a data frane with columns role and message
    
    compose_llm_message_inner <- function(role, message, model){
        
        l_roles <- list(
            gemini = c(assistant = "model",
                       user = "user"),
            default = c(assistant = "assistant",
                        user = "user",
                        system = "system")
        )
        
        # In principle, the format is 
        # list(
        #     list(role = l_roles[["default"]][role] %>% unname,
        #          content = message
        #     )
        # )
        # However, as we call the function from within pmap, the outer list is already provided. 
        if(str_detect(model, "gemini")){
            #list( 
                list(role = l_roles[["gemini"]][role] %>% unname,
                     parts = list(list(text = message))
                )
            #)
        } else {
            #list(
            list(role = l_roles[["default"]][role] %>% unname,
                 content = message
            )
            #)
        }
        
    }
    
    dat %>% 
        purrr::pmap(~ compose_llm_message_inner(..1, ..2, model = model))
}

get_api_response_incremental <- function(dat = ., model, system_prompt, system_instruction, DEBUG = FALSE, JUST_COUNT_CHARACTERS = FALSE){
    
    
    url <- get_model_url(model)
    

    # Get first response. This depends on the model
    i <- 1
    if(str_detect(str_to_lower(model), "gpt")){
        # Don't override openai system prompt
        messages <- tibble::tribble(
            ~role, ~message,
            "user", glue::glue("{system_prompt} 
                               
                               {dat$prompt[i]}")
        ) %>% 
            compose_llm_messages(model = model)
        
    } else if(str_detect(str_to_lower(model), "gemini|claude")){
        
        # System prompt is included in API call
        
        messages <- tibble::tribble(
            ~role, ~message,
            "user", dat$prompt[i]
        ) %>% 
            compose_llm_messages(model = model)
        
    } else {
        messages <- tibble::tribble(
            ~role, ~message,
            "system", system_prompt,
            "user", dat$prompt[i]
        ) %>% 
            compose_llm_messages(model = model)
    } 
    
    if(nrow(dat) > 1){
        messages <- c(messages,
                      tibble::tribble(
                          ~role, ~message,
                          "assistant", dat$response[i]
                      ) %>% 
                          compose_llm_messages(model = model)
        )
    }
    
    if(nrow(dat) > 2){
        # Get subsequent responses except for the very last one
        messages <- c(messages,
                      tibble::tibble(
                          role = rep(c("user", "assistant"), times = nrow(dat) - 2),
                          message = list(dat$prompt[2:(nrow(dat) - 1)], 
                                      dat$response[2:(nrow(dat) - 1)]) %>% 
                              purrr::pmap(~ c(..1, ..2)) %>% 
                              unlist(use.names = FALSE)
                      ) %>% 
                          compose_llm_messages(model = model)
        )
    }

    
    # Last row for which we don't have a response yet
    if(nrow(dat) > 1){
        i <- nrow(dat)
        messages <- c(messages,
                      tibble::tribble(
                          ~role, ~message,
                          "user", dat$prompt[i]
                      ) %>% 
                          compose_llm_messages(model = model)
        )
    }
    
    # Send off the response
    result <- send_llm_request(model, url, messages, system_prompt = system_prompt, system_instruction = system_instruction, DEBUG = DEBUG, JUST_COUNT_CHARACTERS = JUST_COUNT_CHARACTERS) %>% 
         str_replace("^(\\d+)(\\D*)", "\\1")
    
    result
    
}


    
get_api_response_wrapper_df <- function(dat = ., dat_grp, verbose = FALSE, DEBUG = FALSE, JUST_COUNT_CHARACTERS = FALSE, ...){
    
    gc()
    

    model <- dat_grp$model %>% unique
    if(length(model) > 1) stop("More than one model detected")
    
    subj <- dat_grp$subj %>% unique
    if(length(subj) > 1) stop("More than one subject detected")
    
    experimentID <- dat_grp$experimentID %>% unique
    if(length(experimentID) > 1) stop("More than one experiment detected")
    
    if(str_detect(experimentID, "exp2")){
        system_prompt <- SYSTEM_PROMPTS$comparison
        system_instruction <- SYSTEM_INSTRUCTIONS$comparison
    } else {
        system_prompt <- SYSTEM_PROMPTS$estimation
        system_instruction <- SYSTEM_INSTRUCTIONS$estimation
    }
    
    if(isTRUE(verbose)) 
        print(glue::glue("Running model {model} for experiment {experimentID} and subject {subj}."))
    
    
    # Prepare return df
    dat <- dat %>% 
        dplyr::mutate(response = NA_character_)

    for(i in seq_len(nrow(dat))){
        dat[i, "response"] <- tryCatch({
                           res <- get_api_response_incremental(
                               dat[seq_len(i),],
                               model = model,
                               system_prompt = system_prompt,
                               system_instruction = system_instruction,
                               DEBUG = DEBUG,
                               JUST_COUNT_CHARACTERS = JUST_COUNT_CHARACTERS
                               )
                           if (length(res) == 0) NA_character_ else res
                           },
                              
                           error = function(e) NA_character_
                       )  

    }
    
        
    
    return(dat)
}




## ----run-exp-updated--------------------------------------------------------------------------




if(isTRUE(RECALCULATE_LLM$local)){

    # When run through Rscript, llama3.2 takes 684 s/11.4 min per subject (after application of the latin square design, so 10 or 12 subjects in total, depending on the experiment).
    # With 10 replicates of each cell (i.e., 100 or 120 subjects), this would take roughly 2h per model, and 4h for llama and mistral-nemo

    dat_llm_results_exps_local <- dat_scenarios_numbers_flat %>% 
        tidyr::expand_grid(model = MODELS$local) %>% 
        dplyr::group_by(model, experimentID, experimentDesc, condition, Group, lsd_order, combo_id, subj) %>% 
        dplyr::group_modify(~ get_api_response_wrapper_df(.x, .y, verbose = TRUE, DEBUG = FALSE),
                            .keep = FALSE) %>% 
        tidyr::unnest(response) %>% 
        dplyr::mutate(response = as.numeric(response)) %>% 
        dplyr::rename(rating.raw = response) %>% 
        dplyr::rename_with(~ str_replace_all(.x, "_", "."), matches("n_"))  %>% 
        tidyr::drop_na(rating.raw) %>% 
        dplyr::mutate(dplyr::across(all_of(c("n.victims", "n.saved")), as.integer),
                      rating.bin = 1 * (rating.raw > 3),
                      n.net.saved =  n.saved - n.victims,
                      n.total = n.victims + n.saved) 
    
    save(
        dat_llm_results_exps_local,
        file = paste0("output/", "llm_test.all_exps.local_models.RData"))
} else {
    load(paste0("output/", "llm_test.all_exps.local_models.RData"))
}
 

if(isTRUE(RECALCULATE_LLM$cloud)){   
    
    # Parellelize only by model
    # We save model specific RData files from within the loop as well as a general RData file with some reformatting
    
    safe_get_api_response <- purrr::safely(get_api_response_wrapper_df, otherwise = NULL)
    
    dat_llm_results_exps_cloud <- furrr::future_map_dfr(
        MODELS$cloud,
        function(model){
            Sys.sleep(runif(1, 0.1, 0.5))  # jittered sleep
            
            partial_result <- dat_scenarios_numbers_flat %>% 
                dplyr::mutate(model = model) %>% 
                dplyr::group_by(model, experimentID, experimentDesc, condition, Group, lsd_order, combo_id, subj) %>% 
                dplyr::group_modify(
                    function(dat, dat_grp){
                        
                        res <- safe_get_api_response(dat, dat_grp, verbose = TRUE, DEBUG = FALSE)
                        
                        if (!is.null(res$error)) {
                            tibble::tibble(
                                model = dat_grp$model,
                                experimentID = dat_grp$experimentID,
                                error = paste("Error:", res$error$message)
                            )
                        } else {
                            res$result
                        }
                    },
                    .keep = FALSE)
            
            # Save model specific data file
            result_var_name <- glue::glue("dat_llm_results_exps_cloud_{gsub('-', '_', model)}")
            result_file_name <- glue::glue("llm_test.all_exps.{model}.RData")
            # Don't care if it's in the global environment or not as it will be saved anyhow
            assign(result_var_name,
                   partial_result, 
                   envir = .GlobalEnv)
            
            save(list = result_var_name,
                 file = file.path("output", result_file_name))
            
            partial_result
        }
        
    ) %>% 
        tidyr::unnest(response, keep_empty = TRUE) %>% 
        dplyr::mutate(response = as.numeric(response)) %>% 
        dplyr::rename(rating.raw = response) %>% 
        dplyr::rename_with(~ str_replace_all(.x, "_", "."), matches("n_"))  %>% 
        tidyr::drop_na(rating.raw) %>% 
        dplyr::mutate(dplyr::across(all_of(c("n.victims", "n.saved")), as.integer),
                      rating.bin = 1 * (rating.raw > 3),
                      n.net.saved =  n.saved - n.victims,
                      n.total = n.victims + n.saved) 
    

    # save general data file
    # Commented out as we don't seem to use it anywhere    
        
    # save(
    #     dat_llm_results_exps_cloud, 
    #     file = paste0("output/", "llm_test.all_exps.cloud_models.RData"))
    
    
} else {
    
    
    # load general data file
    # Commented out as we don't seem to use it anywhere    
    # load(paste0("output/", "llm_test.all_exps.cloud_models.RData"))
    
    
    l_cloud_result_vars <- purrr::map(MODELS$cloud,
                                       function(model){
                                           
                                           file <- file.path("output/",
                                                             glue::glue("llm_test.all_exps.{model}.RData"))
                                           
                                           if(!file.exists(file)) return(NA_character_)
                                           
                                           tmp_env <- new.env()
                                           load(file, envir = tmp_env)
                                           var_names_in_env <- ls(tmp_env)
                                           
                                           walk(var_names_in_env,
                                                ~ assign(.x, tmp_env[[.x]], envir = .GlobalEnv)
                                           )
                                           
                                           var_names_in_env
                                       }
    ) %>% 
        purrr::flatten_chr() %>% 
        purrr::discard(is.na)
    


        
}



