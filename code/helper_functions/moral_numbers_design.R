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