
create.fit.plot <- function(dat = ., dat.fit, x.var = ratio, value.var = rating.bin, group.var = NULL, xlab = "Weber ratio", ylab = "Acceptability"){
    
    x.var <- enquo(x.var)
    value.var <- enquo(value.var)
    group.var <- enquo(group.var)
    
    
    if(!rlang::quo_is_null(group.var)){
        group.levels <- dat %>% 
            pull(!!group.var) %>% 
            levels2
    }
    
    
    plot.fit <-  dat %>% 
        dplyr::group_by(experimentID, ResponseId, !!group.var, !!x.var) %>% 
        dplyr::summarize(!!value.var := mean(!!value.var),
                         .groups = "drop") %>% 
        dplyr::group_by(experimentID, !!group.var, !!x.var) %>% 
        # dplyr::summarize(rating.m = mean(!!value.var),
        #                  rating.se = se(!!value.var)) %>% 
        # ggplot (aes(x = !!x.var, y = rating.m, ymin = rating.m - rating.se, ymax = rating.m + rating.se, col = !!group.var, group = !!group.var)) +
        dplyr::summarize(rating = Hmisc::smean.cl.boot(!!value.var) %>% t %>% as.data.frame) %>% 
        tidyr::unnest(rating) %>% 
        ggplot (aes(x = !!x.var, y = Mean, ymin = Lower, ymax = Upper, col = !!group.var, group = !!group.var)) +        
        
        
        geom_pointrange()
    
    if(rlang::quo_is_null(group.var)){
        plot.fit <- plot.fit +
            stat_function(#col = "red",
                #                   size = 2,
                fun = function(x) acceptability.fnc(
                    w = dat.fit %>% 
                        pull(w),
                    a = switch(assertthat::has_name(dat.fit, "a") + 1,
                               NULL,
                               dat.fit %>% 
                                   pull(a)),
                    #a = 1.5,
                    ratio = x))
        
    } else {
        
        # Get the current colors
        v.current.colors <- ggplot_build(plot.fit )$data[[1]] %>% 
            pull (colour) %>% 
            unique()
        
        
        plot.fit <- plot.fit +
            #scale_color_manual(values = c("red", "blue")) +
            stat_function(col = v.current.colors[1],
                          #                   size = 2,
                          fun = function(x) acceptability.fnc(
                              w = dat.fit %>% 
                                  dplyr::filter(!!group.var == group.levels[1]) %>% 
                                  pull(w),
                              a = switch(assertthat::has_name(dat.fit, "a") + 1,
                                         NULL,
                                         dat.fit %>% 
                                             dplyr::filter(!!group.var == group.levels[1]) %>% 
                                             pull(a)),
                              #a = 1.5,
                              ratio = x)) +
            stat_function(col = v.current.colors[2],
                          #                   size = 2,
                          fun = function(x) acceptability.fnc(
                              w = dat.fit %>% 
                                  dplyr::filter(!!group.var == group.levels[2]) %>% 
                                  pull(w),
                              a = switch(assertthat::has_name(dat.fit, "a") + 1,
                                         NULL,
                                         dat.fit %>% 
                                             dplyr::filter(!!group.var == group.levels[2]) %>% 
                                             pull(a)),
                              #a = 1.5,
                              ratio = x)) 
        
    }
    
    plot.fit +
        labs(x = xlab,
             y = ylab) + 
        theme(legend.title = element_blank())
    
    
}

# create argument dat.fit for create.fit.plot
get_dat_fit_for_plot <- function(..., n_fit_params = 1, fit_type = c("overall", "bootstrap", "linear"), DEBUG = FALSE){
    
    
    get_condition_var <- function(dat){
        
        condition_var <- dat %>% 
            dplyr::select(decisionType, vivacity) %>% 
            # for which column are not all elements NA
            dplyr::summarise(across(everything(), ~ !all(is.na(.x)))) %>% 
            tidyr::pivot_longer(cols = everything(), names_to = "condition_var", values_to = "use") %>% 
            dplyr::filter(use) %>% 
            dplyr::pull(condition_var)
        
        if(length(condition_var) > 1) {
            
            stop(
                glue::glue(
                    "Detection of decisionType or vivacity failed. ",
                    "condition_var should have only one value, but I found {length(condition_var)}: {toString(condition_var)}.\n\n",
                    "This is based on the input data frame:\n{paste(capture.output(print(dat)), collapse = '\n')}"
                ),
                call. = TRUE
            )
        }
        
        condition_var
        
    }
    
    fit_type <- match.arg(fit_type)
    
    # We assume here that all relevant data frames and flags are global, but check this nonetheless
    c("dat.moral.numbers.overall.fit.1param",
      "dat.moral.numbers.overall.fit.2param",
      "dat.moral.numbers.overall.fit.1param.linearized",
      "dat.moral.numbers.overall.fit.2param.linearized",
      "dat.moral.numbers.bootstrap.1param.summary",
      "dat.moral.numbers.bootstrap.2param.summary") %>% 
        purrr::walk(~ if(!exists(.x)) stop("object ", .x , " does not exist.", call. = TRUE))
    
    # create base df
    if (fit_type == "overall" && n_fit_params == 1) {
        dat_base <- dat.moral.numbers.overall.fit.1param
    } else if (fit_type == "overall" && n_fit_params == 2) {
        dat_base <- dat.moral.numbers.overall.fit.2param
    } else if (fit_type == "linear" && n_fit_params == 1) {
        dat_base <- dat.moral.numbers.overall.fit.1param.linearized
    } else if (fit_type == "linear" && n_fit_params == 2) {
        dat_base <- dat.moral.numbers.overall.fit.2param.linearized        
    } else if (fit_type == "bootstrap" && n_fit_params == 1) {
        dat_base <- dat.moral.numbers.bootstrap.1param.summary
    } else if (fit_type == "bootstrap" && n_fit_params == 2) {
        dat_base <- dat.moral.numbers.bootstrap.2param.summary
    } else {
        stop("No matching data found for the combination of fit_type and n_fit_params.")
    }
    
    dat_base <- dat_base %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(...) %>% 
        dplyr::mutate(decisionType = str_to_title(decisionType)) %>% 
        dplyr::mutate(vivacity = str_to_title(vivacity))
    
    # get_relevant condition 
    condition_var <- get_condition_var(dat_base)
    
    if(fit_type == "overall" | fit_type == "linear"){
        
        dat_fit <- dat_base %>% 
            tidyr::pivot_wider(id_cols = condition_var,
                               names_from = term,
                               values_from = estimate) 
        
    } else if(fit_type == "bootstrap") {
        
        dat_fit <- dat_base %>% 
            dplyr::select(condition_var, ends_with("_M")) %>% 
            dplyr::rename_with(~ str_remove(.x, "_M"), ends_with("_M"))
        
    }
    
    
    dat_fit
}


# Convert the summaries from the bootstrap fits to those that can be used by create.fit.plot    
bootstrap.summary.to.fit.summary <- function(dat = .){
    dat %>% 
        tidyr::pivot_longer(ends_with("_M"),
                            names_to = "term",
                            values_to = "estimate") %>% 
        dplyr::select(-matches("_")) %>% 
        dplyr::mutate(term = str_remove(term, "_M$"))
}

create.predictor.comparison.plot <- function(dat = ., dat.fit = NULL, value.var = rating.bin, facet.var = NULL, col.var = NULL, ylab = NULL, legend = "bottom", add.fit = FALSE, return.plot = TRUE, add.p.saved.to.list = FALSE){
    
    
    create.predictor.comparison.plot.inner.function <- function(dat = ., x.var, value.var = rating.bin, facet.var = NULL, col.var = NULL, xlab = NULL, ylab = NULL, yintercept, current.plot.margin){
        
        
        x.var <- dplyr::enquo(x.var)
        value.var <- dplyr::enquo(value.var)
        facet.var <- dplyr::enquo(facet.var)
        col.var <- dplyr::enquo(col.var)
        
        
        dat %>% 
            dplyr::group_by(experimentID, ResponseId, !!facet.var, !!col.var, !!x.var) %>% 
            dplyr::summarize(!!value.var := mean(!!value.var)) %>% 
            ggplot (aes (x = !!x.var, y = !!value.var, col = !!col.var)) +
            labs(x = xlab, y = ylab) +
            #        theme_linedraw(14) + 
            scale_x_continuous(xlab,
                               #labels = ~ str_wrap(.x, 15),
                               guide = guide_axis(angle = 60)) + 
            #scale_y_continuous("Rating (raw)") +# , limits = 0:1) + 
            stat_summary(fun.data = mean_cl_boot, 
                         #fun.args = list(mult=sqrt (nlevels2(.$filename)-1)), 
                         geom = "pointrange") + 
            geom_hline(yintercept = yintercept, lty = 3) +
            facet_grid(cols = vars(!!facet.var), scales = "free_y") + 
            theme(legend.title = element_blank()) + 
            theme(plot.margin = current.plot.margin)
        
        
    }
    
    
    
    if(is.null(dat.fit) && add.fit){
        stop("dat.fit needs to be specified when a fit is to be plotted.")    
    }
    
    if(return.plot && (!add.p.saved.to.list)){
        warning("When a plot is returned, it will include a panel for the proportion of saved.")
    }
    
    value.var <- dplyr::enquo(value.var)
    facet.var <- dplyr::enquo(facet.var)
    col.var <- dplyr::enquo(col.var)
    
    if (max(dat %>% pull(!!value.var)) <= 1){
        yintercept <- .5
    } else {
        yintercept <- 3.5
    }
    
    #Margin order: top, right, bottom, and left 
    current.plot.margin <- theme_get()$plot.margin
    
    # Use defaults if NULL
    if(is.null(current.plot.margin)) {
        current.plot.margin <- margin(5.5, 5.5, 5.5, 5.5, "pt")
    }
    
    if(legend != "none"){
        # Bigger margins
        # current.plot.margin <- theme_get()$plot.margin * c(2, 1, 1, 2)
        
        # create new margin (top, right, bottom, left)
        current.plot.margin <- current.plot.margin * c(2, 1, 1, 2) 
        
    }
    
    # Create plot by ratio
    if (add.fit) {
        if(is.null(col.var)) {
            plot.by.ratio <- dat %>% 
                create.fit.plot(dat.fit = dat.fit, x.var = ratio, value.var = !!value.var, xlab = "Weber ratio", ylab = ylab) + 
                theme(plot.margin = current.plot.margin)
            
        } else {
            
            plot.by.ratio <- dat %>% 
                create.fit.plot(dat.fit = dat.fit, x.var = ratio, value.var = !!value.var, group.var = !!col.var, xlab = "Weber ratio", ylab = ylab) + 
                theme(plot.margin = current.plot.margin)
        }
        
        
    } else {
        plot.by.ratio <- dat %>% 
            create.predictor.comparison.plot.inner.function(x.var = ratio, value.var = !!value.var, facet.var = !!facet.var, col.var = !!col.var, xlab = "Weber ratio", ylab = ylab, yintercept = yintercept, current.plot.margin = current.plot.margin)
    }
    
    # Create plot by proportion of saved
    plot.by.p.saved <- dat %>% 
        dplyr::mutate(p.saved = n.saved / n.total) %>% 
        create.predictor.comparison.plot.inner.function(x.var = p.saved, value.var = !!value.var, facet.var = !!facet.var, col.var = !!col.var, xlab = "Proportion of beneficiaries", ylab = ylab, yintercept = yintercept, current.plot.margin = current.plot.margin)
    
    
    # Create plot by number of victims
    plot.by.n.vicitms <- dat %>% 
        create.predictor.comparison.plot.inner.function(x.var = n.victims, value.var = !!value.var, facet.var = !!facet.var, col.var = !!col.var, xlab = "Harm", ylab = ylab, yintercept = yintercept, current.plot.margin = current.plot.margin)
    
    
    
    # Craeate plot by utility (net number of saved)
    p.by.n.net.saved <- dat %>% 
        create.predictor.comparison.plot.inner.function(x.var = n.net.saved, value.var = !!value.var, facet.var = !!facet.var, col.var = !!col.var, xlab = "Utility", ylab = ylab, yintercept = yintercept, current.plot.margin = current.plot.margin)
    
    if (return.plot){
        ggpubr::ggarrange(
            plot.by.ratio,
            plot.by.p.saved,
            plot.by.n.vicitms,
            p.by.n.net.saved,
            nrow = 2,
            ncol = 2,
            labels = "auto",
            common.legend = TRUE,
            legend = legend)
    } else {
        
        if (add.p.saved.to.list){
            
            list(plot.by.ratio = plot.by.ratio,
                 plot.by.p.saved = plot.by.p.saved,
                 plot.by.n.vicitms = plot.by.n.vicitms,
                 p.by.n.net.saved = p.by.n.net.saved)
            
        } else {
            
            list(plot.by.ratio = plot.by.ratio,
                 plot.by.n.vicitms = plot.by.n.vicitms,
                 p.by.n.net.saved = p.by.n.net.saved)
        }
    }
}


combine_vivacity_detailed_plots <- function(experimentID){
    filter_str <- ifelse(stringr::str_detect(experimentID, "vivacityManipulationWorking"), "Filtered", "Unfiltered")

    purrr::pmap(
        list(
            # Plots to combine
            list(
                purrr::pluck(l.exp.vivacity.model.comp.plots.bin, experimentID, "all", "plot.by.ratio"),
                purrr::pluck(l.exp.vivacity.model.comp.plots.bin, experimentID, "first", "plot.by.ratio"),
                purrr::pluck(l.exp.vivacity.model.comp.plots.raw, experimentID, "all", "plot.by.ratio"),
                purrr::pluck(l.exp.vivacity.model.comp.plots.raw, experimentID, "first", "plot.by.ratio")
            ),

            # Titles
            stringr::str_c(filter_str,
                           c(
                               ", binarized, both blocks",
                               ", binarized, first block",
                               ", raw, both blocks",
                               ", raw, first block"
                           )
            ),

            # Experiment IDs
            rep(experimentID, 4)
        ),
        ~ ..1 +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
                axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 12)),
                legend.position = "bottom",
                plot.title = ggplot2::element_text(size = 10)
            ) +
            ggplot2::labs(title = stringr::str_wrap(..2, 25)) +
            ggplot2::scale_x_continuous(
                breaks =
                    purrr::pluck(
                        l.moral.numbers.ratios,
                        stringr::str_remove(..3, "\\.vivacityManipulationWorking")
                    ),
                trans = "log2",
                guide = ggplot2::guide_axis(check.overlap = TRUE)
            )
    ) %>%
        ggpubr::ggarrange(
            plotlist = .,
            ncol = 2, nrow = 2,
            labels = "auto",
            legend = "bottom",
            common.legend = TRUE
        )
}
