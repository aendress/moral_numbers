# create argument dat.fit for create.fit.plot

get_dat_fit_for_plot_llm <- function(..., n_fit_params = 1, DEBUG = FALSE){
    
    
    
    
    # We assume here that all relevant data frames and flags are global, but check this nonetheless
    c("dat_llm_fits_1_param",
      "dat_llm_fits_2_param") %>% 
        purrr::walk(~ if(!exists(.x)) stop("object ", .x , " does not exist.", call. = TRUE))
    
    # create base df
    if (n_fit_params == 1) {
        dat_base <- dat_llm_fits_1_param
    } else if (n_fit_params == 2) {
        dat_base <- dat_llm_fits_1_param
    } else {
        stop("No matching data found for the combination of fit_type and n_fit_params.")
    }
    
    dat_base <- dat_base %>% 
        dplyr::ungroup() %>% 
        dplyr::filter(...) %>% 
        dplyr::mutate(condition = relabel_conds_llm(condition))
    
    dat_fit <- dat_base %>%
        tidyr::pivot_wider(id_cols = c(condition, model),
                           names_from = term,
                           values_from = estimate)
    
    
    dat_fit
}

create.fit.plot.llm <- function(dat = ., dat.fit, x.var = ratio, value.var = rating.bin, group.var = NULL, facet.var = NULL, xlab = "Weber ratio", ylab = "Acceptability"){
    
    x.var <- enquo(x.var)
    value.var <- enquo(value.var)
    group.var <- enquo(group.var)
    facet.var <- enquo(facet.var)
    
    if(is.null(dat.fit)){
        stop("dat.fit must be provided")
    }
    
    if(!rlang::quo_is_null(group.var)){
        if(is.factor(pull(dat, !!group.var))){
            group.levels <- dat %>%
                pull(!!group.var) %>%
                droplevels() %>%
                levels()
        } else {
            group.levels <- dat %>%
                pull(!!group.var) %>%
                levels2
        }
    }
    
    
    if(!rlang::quo_is_null(facet.var)){
        if(is.factor(pull(dat, !!facet.var))){
            facet.levels <- dat %>%
                pull(!!facet.var) %>%
                droplevels() %>%
                levels()
        } else {
            facet.levels <- dat %>%
                pull(!!facet.var) %>%
                levels2
        }
    }
    
    # Summarize data
    dat_fit <- dat %>% 
        dplyr::group_by(experimentID, ResponseId, !!group.var, !!facet.var, !!x.var) %>% 
        dplyr::summarize(!!value.var := mean(!!value.var),
                         .groups = "drop") %>% 
        dplyr::group_by(experimentID, !!group.var, !!facet.var,, !!x.var) %>% 
        dplyr::summarize(rating = Hmisc::smean.cl.boot(!!value.var) %>% 
                             t %>% 
                             as.data.frame,
                         .groups = "drop") %>%
        tidyr::unnest(rating) 
    
    
    plot.fit <- dat_fit %>%
        ggplot2::ggplot(ggplot2::aes(x = !!x.var, y = Mean, ymin = Lower, ymax = Upper, col = !!group.var, group = !!group.var)) + ggplot2::geom_pointrange()
    #geom_point()
    
    
    # Precompute predictions instead of having them computed by ggplot
    if(!rlang::quo_is_null(group.var) && !rlang::quo_is_null(facet.var)){
        
        
        # Precompute predictions for each group
        
        dat_pred <- tidyr::expand_grid(
            grp = group.levels,
            fctl = facet.levels,
        ) %>% 
            purrr::pmap_dfr(
                \(grp, fctl) {
                    
                    fit.params <- dat.fit %>% 
                        dplyr::filter(!!group.var == grp,
                                      !!facet.var == fctl)
                    
                    # Debug: print if fit.params is empty
                    if (nrow(fit.params) == 0) {
                        warning("⚠️ Missing fit.params for group: ", grp, 
                                ", facet: ", fctl)
                        return(tibble())
                    }
                    
                    tibble(
                        !!group.var := grp,
                        !!facet.var := fctl,
                        !!x.var := seq(min(dat[[quo_name(x.var)]]), max(dat[[quo_name(x.var)]]), length.out = 100), 
                        pred = 
                            if(is.na(fit.params$w)){
                                rep(NA_real_, 100)
                            } else {
                                acceptability.fnc(
                                    w = fit.params$w,
                                    a = if ("a" %in% names(fit.params)) fit.params$a else NULL,
                                    #ratio = .data[[quo_name(x.var)]]
                                    ratio = !!x.var
                                )
                            }
                    )
                }
            ) %>% 
            dplyr::mutate(
                !!group.var := factor(!!group.var, levels = group.levels),
                !!facet.var := factor(!!facet.var, levels = facet.levels)
            )
        
    } else {
        # Single fit without grouping
        dat_pred <- tibble(
            !!x.var := seq(min(dat[[quo_name(x.var)]]), max(dat[[quo_name(x.var)]]), length.out = 100),
            pred = acceptability.fnc(
                w = dat.fit$w,
                a = if ("a" %in% names(dat.fit)) dat.fit$a else NULL,
                #ratio = .data[[quo_name(x.var)]]
                ratio = !!x.var
            )
        )
    }    
    
    # Add lines for predictions
    if(!rlang::quo_is_null(group.var)){
        
        
        plot.fit <- plot.fit +
            ggplot2::geom_line(data = dat_pred, ggplot2::aes(x = !!x.var, y = pred, col = !!group.var, group = !!group.var),
                               inherit.aes = FALSE,
                               show.legend = FALSE)
    } else {
        
        plot.fit <- plot.fit +
            ggplot2::geom_line(data = dat_pred, ggplot2::aes(x = !!x.var, y = pred), inherit.aes = FALSE)
    }
    
    
    plot.fit +
        ggplot2::labs(x = xlab,
                      y = ylab) +
        ggplot2::theme(legend.title = ggplot2::element_blank())
    
    
}


create.predictor.comparison.plot.llm <- function(dat = ., dat.fit = NULL, value.var = rating.bin, facet.var = NULL, col.var = NULL, ylab = NULL, legend = "bottom", add.fit = FALSE, return.plot = TRUE, add.p.saved.to.list = FALSE){
    
    
    create.predictor.comparison.plot.inner.function <- function(dat = ., x.var, value.var = rating.bin, facet.var = NULL, col.var = NULL, xlab = NULL, ylab = NULL, yintercept, current.plot.margin){
        
        
        x.var <- dplyr::enquo(x.var)
        value.var <- dplyr::enquo(value.var)
        facet.var <- dplyr::enquo(facet.var)
        col.var <- dplyr::enquo(col.var)
        
        
        dat %>% 
            dplyr::group_by(experimentID, ResponseId, !!facet.var, !!col.var, !!x.var) %>% 
            dplyr::summarize(
                !!value.var := mean(!!value.var),
                .groups = "drop"
            ) %>% 
            ggplot2::ggplot(ggplot2::aes(x = !!x.var, y = !!value.var, col = !!col.var)) +
            ggplot2::labs(x = xlab, y = ylab) +
            #        theme_linedraw(14) +
            ggplot2::scale_x_continuous(xlab,
                                        #labels = ~ str_wrap(.x, 15),
                                        guide = ggplot2::guide_axis(angle = 60)) +
            #scale_y_continuous("Rating (raw)") +# , limits = 0:1) +
            stat_summary(fun.data = mean_cl_boot,
                         #fun.args = list(mult=sqrt (nlevels2(.$filename)-1)),
                         geom = "pointrange") +
            ggplot2::geom_hline(yintercept = yintercept, lty = 3) +
            ggplot2::facet_grid(cols = vars(!!facet.var), scales = "free_y") +
            ggplot2::theme(legend.title = ggplot2::element_blank()) +
            ggplot2::theme(plot.margin = current.plot.margin)
        
        
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
        current.plot.margin <- ggplot2::margin(5.5, 5.5, 5.5, 5.5, unit = "pt")
    }
    
    if(legend != "none"){
        # Bigger margins
        
        # create new margin (top, right, bottom, left)
        current.plot.margin <- current.plot.margin * c(2, 1, 1, 2) 
        
        #        current.plot.margin <- theme_get()$plot.margin * c(2, 1, 1, 2)
    }
    
    # Create plot by ratio
    if (add.fit) {
        if(is.null(col.var)) {
            plot.by.ratio <- dat %>%
                create.fit.plot.llm(dat.fit = dat.fit, x.var = ratio, value.var = !!value.var, xlab = "Weber ratio", ylab = ylab) +
                ggplot2::theme(plot.margin = current.plot.margin)
            
        } else {
            # Pass facet.var as well
            plot.by.ratio <- dat %>%
                create.fit.plot.llm(dat.fit = dat.fit, x.var = ratio, value.var = !!value.var, group.var = !!col.var,
                                    facet.var = !!facet.var, # Addition for LLM code
                                    xlab = "Weber ratio", ylab = ylab) +
                ggplot2::theme(plot.margin = current.plot.margin)
        }
        
        
    } else {
        plot.by.ratio <- dat %>% 
            create.predictor.comparison.plot.inner.function(x.var = ratio, value.var = !!value.var, facet.var = !!facet.var, col.var = !!col.var, xlab = "Weber ratio", ylab = ylab, yintercept = yintercept, current.plot.margin = current.plot.margin)
    }
    
    # Create plot by proportion of saved
    plot.by.p.saved <- dat %>% 
        dplyr::mutate(p.saved = n.saved / n.total) %>% 
        create.predictor.comparison.plot.inner.function(x.var = p.saved, value.var = !!value.var, facet.var = !!facet.var, col.var = !!col.var, xlab = "Proportion saved", ylab = ylab, yintercept = yintercept, current.plot.margin = current.plot.margin)
    
    
    # Create plot by numbver of victims
    plot.by.n.vicitms <- dat %>% 
        create.predictor.comparison.plot.inner.function(x.var = n.victims, value.var = !!value.var, facet.var = !!facet.var, col.var = !!col.var, xlab = "Number of victims", ylab = ylab, yintercept = yintercept, current.plot.margin = current.plot.margin)
    
    
    
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

make_llm_exp2_joint_plot <- function(base_size = 14, label_size = 3, add_repel = FALSE, repel_size = label_size) {
    
    # Inner function: adds condition-specific quadrant frame and labels to a plot
    add_quadrants <- function(p) {
        
        dat_quadrant_labels <- tibble::tribble(
            ~condition,  ~x,    ~y, ~hjust, ~vjust, ~label,
            "Moral",     2.25,  6,  0.5,    1,      "Ratio < Harm\nRatio > Utility",
            "Moral",     4.75,  6,  0.5,    1,      "Ratio > Harm\nRatio > Utility",
            "Moral",     2.25,  1,  0.5,    0,      "Ratio < Harm\nRatio < Utility",
            "Moral",     4.75,  1,  0.5,    0,      "Ratio > Harm\nRatio < Utility",
            "Economic",  2.25,  6,  0.5,    1,      "Ratio < Harm\nRatio > Utility",
            "Economic",  4.75,  6,  0.5,    1,      "Ratio > Harm\nRatio > Utility",
            "Economic",  2.25,  1,  0.5,    0,      "Ratio < Harm\nRatio < Utility",
            "Economic",  4.75,  1,  0.5,    0,      "Ratio > Harm\nRatio < Utility"
        ) %>% 
            dplyr::mutate(condition = factor(condition, levels = c("Moral", "Economic")))
        
        # Extract actual panel extent to bleed rect borders into margins
        panel_params <- ggplot2::ggplot_build(p)$layout$panel_params[[1]]
        
        # Human-like quadrant: upper-right for Moral, lower-right for Economic
        # Inset outer edges by ~1mm in data units so the border isn't clipped
        margin <- 0.02
        dat_human_quadrant <- tibble::tribble(
            ~condition,  ~xmin, ~xmax,                                ~ymin,                             ~ymax,
            "Moral",     3.5,   panel_params$x.range[2] - margin,    3.5,                               panel_params$y.range[2] - margin,
            "Economic",  3.5,   panel_params$x.range[2] - margin,    panel_params$y.range[1] + margin,  3.5
        ) %>%
            dplyr::mutate(condition = factor(condition, levels = c("Moral", "Economic")))
        
        p +
            ggplot2::geom_rect(
                data = dat_human_quadrant,
                ggplot2::aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax),
                fill        = NA,
                color       = "#0072B2",
                linewidth   = 1,
                inherit.aes = FALSE
            ) +
            ggplot2::geom_text(
                data = dat_quadrant_labels,
                ggplot2::aes(x = x, y = y, hjust = hjust, vjust = vjust, label = label),
                size        = label_size,
                color       = "gray20",
                inherit.aes = FALSE
            )
    }
    
    # Build base plot first (without quadrant frame) so we can extract panel extent
    p_joint_plot_base <-
        dat_llm_results_exp2_descriptives_for_plot_wide %>%
        ggplot2::ggplot(ggplot2::aes(
            x     = `M_Exp. 3a (Ratio vs. harm)`,
            y     = `M_Exp. 3b (Ratio vs. utility)`,
            xmin  = `ci.lower_Exp. 3a (Ratio vs. harm)`,
            xmax  = `ci.upper_Exp. 3a (Ratio vs. harm)`,
            ymin  = `ci.lower_Exp. 3b (Ratio vs. utility)`,
            ymax  = `ci.upper_Exp. 3b (Ratio vs. utility)`,
            label = model,
            color = model_type,
            shape = model_type
        )) +
        # Reference crosshair
        ggplot2::geom_vline(xintercept = 3.5, linetype = "dashed", color = "grey50") +
        ggplot2::geom_hline(yintercept = 3.5, linetype = "dashed", color = "grey50") +
        # 95% CI error bars
        ggplot2::geom_errorbar(
            height = 0.15, linewidth = 0.4,
            orientation = "x") +
        ggplot2::geom_errorbar(
            width = 0.15, linewidth = 0.4,
            orientation = "y") +
        ggplot2::geom_point(ggplot2::aes(size = model_type)) +
        ggplot2::scale_color_manual(
            values = c("Humans" = "#0072B2", "Frontier" = "#E69F00", "Local" = "#999999"),
            name   = "Model type"
        ) +
        ggplot2::scale_shape_manual(
            values = c("Humans" = 18, "Frontier" = 16, "Local" = 17),
            name   = "Model type"
        ) +
        ggplot2::scale_size_manual(
            values = c("Humans" = 4,  "Frontier" = 3,  "Local" = 3),
            guide  = "none"
        ) +
        ggplot2::coord_cartesian(xlim = c(1, 6), ylim = c(1, 6)) +
        ggplot2::scale_x_continuous(breaks = seq(1, 6)) +
        ggplot2::scale_y_continuous(breaks = seq(1, 6)) +
        ggplot2::facet_wrap(~ condition) +
        ggplot2::labs(
            x = "Exp. 3a (ratio vs. harm)",
            y = "Exp. 3b (ratio vs. utility)"
        ) +
        ggthemes::theme_clean(base_size) +
        ggplot2::theme(
            text         = ggplot2::element_text(size = base_size),
            axis.text    = ggplot2::element_text(size = base_size * 0.8),
            axis.title   = ggplot2::element_text(size = base_size),
            strip.text   = ggplot2::element_text(size = base_size * 0.9),
            legend.text  = ggplot2::element_text(size = base_size * 0.8),
            legend.title = ggplot2::element_text(size = base_size),
            legend.position = "bottom"
        )
    
    p_joint_plot_base <- add_quadrants(p_joint_plot_base)
    
    if(isTRUE(add_repel))
        p_joint_plot_base <- p_joint_plot_base +
        ggrepel::geom_text_repel(
            max.overlaps       = Inf,
            box.padding        = 0.4,
            min.segment.length = Inf,
            seed               = 42,
            size               = repel_size
        )
    
    p_joint_plot_base
}
