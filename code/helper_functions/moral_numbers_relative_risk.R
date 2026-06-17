create_acceptability_contingency_table <- function(dat, rating_col = rating.bin, cond_col = vivacity){

    rating_col <- rlang::enquo(rating_col)
    cond_col <- rlang::enquo(cond_col)
    cond_col_name <- rlang::as_label(cond_col)

    dat %>%
        # make sure it's a factor
        dplyr::mutate(
            !!rating_col := as.factor(!!rating_col)
        ) %>%
        # Create a contingency table of vivacity (rows) by rating.bin (columns),
        # where 0 and 1 in rating.bin represent binary outcomes
        janitor::tabyl(
            !!cond_col,
            !!rating_col,
            show_missing_levels = TRUE
        ) %>%
        # Convert counts to row-wise proportions (i.e., percentage within each vivacity level)
        janitor::adorn_percentages(
            denominator = "row"
        ) %>%
        # drop `0` column as it's redundant
        dplyr::select(
            any_of(c(cond_col_name, "1"))
        ) %>%
        tidyr::pivot_wider(
            id_cols = NULL,
            names_from = !!cond_col,
            values_from = `1`
        )
}


make_rr_neutral_vivid <- function(dat = ., neutral_col = neutral, vivid_col = vivid, rr_col = RR){
    
    neutral_col <- rlang::enquo(neutral_col)
    vivid_col <- rlang::enquo(vivid_col)
    rr_col <- rlang::enquo(rr_col)
    
    dat %>% 
        dplyr::mutate(!!rr_col := dplyr::case_when(
            (!!neutral_col > 0) & (!!vivid_col > 0) ~ !!vivid_col/!!neutral_col,
            (!!neutral_col == 0) & (!!vivid_col == 0) ~ 1,
            (!!neutral_col > 0) & (!!vivid_col == 0) ~ 0,
            (!!neutral_col == 0) & (!!vivid_col > 0) ~ NA_real_))
    
}

create_rr_plot <- function(expID, facets = FALSE){
    
    list(
        # By ratios
        by_ratios = 
            dat.moral.numbers.rr.by.ratios.bootstrap.summary  %>% 
            dplyr::filter(experimentID %in% expID) %>% 
            format_exp_info() %>%
            ggplot(aes(x = ratio, y = RR_M, ymin = RR_pi.lower, ymax = RR_pi.upper)) +        
            geom_pointrange() +
            geom_smooth(method = 'lm', formula = y ~ x) + 
            labs(y = TeX("$\\frac{P_{acceptable, vivid}}{P_{acceptable, neutral}}$")) + 
            { if(isTRUE(facets)) facet_wrap(. ~ experimentID, ncol = 1, scales = "free_x") } +
            { if(length(expID) == 1)
                scale_x_continuous(name = "Weber ratio",
                                   breaks = purrr::pluck(
                                       l.moral.numbers.ratios,
                                       stringr::str_remove(expID, "\\.vivacityManipulationWorking")),
                                   trans = "log2")
              else
                scale_x_continuous(name = "Weber ratio", trans = "log2") } +
            #coord_trans(x = "log2"),
            theme(
                axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1),
                axis.title.x = element_text(margin = margin(t = 12))
            )
        
        
    )
    
}
