create_data_for_prospect_demo <- function(endowment,
                                          relative_changes = seq(-99, 99, 1) / 100,
                                          ws = c(.25, .5, .75))
{
    data.frame(
        endowment = endowment,
        relative_change = relative_changes
    ) %>%
        dplyr::mutate(
            change = endowment * relative_change
        ) %>%
        tidyr::expand_grid(w = ws) %>%
        dplyr::group_by(w) %>%
        dplyr::group_modify(
            \(df, df_w){
                df %>%
                    dplyr::mutate(
                        ratio = dplyr::if_else(
                            change >= 0,
                            (endowment + change) / endowment,
                            endowment / (endowment + change)
                        ),
                        value = dplyr::if_else(
                            change >= 0,
                            acceptability.fnc(w = df_w$w, a = 1, r = ratio),
                            -acceptability.fnc(w = df_w$w, a = 1, r = ratio)
                        )
                    )
            }
        ) %>%
        dplyr::ungroup() %>%
        dplyr::mutate(w = as.factor(w))
}

create_prospect_change_value_plot <- function(dat = .)
{
    ggplot(
        dat,
        aes(x = relative_change, y = value, group = w, col = w, lty = w)
    ) +
        geom_line() +
        labs(
            x = "Relative change (relative to endowment)",
            y = "Utility (arbitrary units)"
        ) +
        ggplot2::scale_color_discrete("Internal noise (w)",  drop = FALSE) +
        ggplot2::scale_linetype_discrete("Internal noise (w)", drop = FALSE) +
        ggh4x::coord_axes_inside(
            xintercept = 0,
            yintercept = 0,
            labels_inside = TRUE,
        ) +
        ggalt::geom_spikelines(
            data = subset(dat, relative_change %in% c(-.50, .50)),
            linetype = "dotted",
            colour = "blue"
        )
}

create_prospect_relative_value_plot <- function(dat = .){
    dat %>%
        dplyr::mutate(
            change_sign = sign(change),
            relative_change = abs(relative_change),
            value = abs(value)
        ) %>%
        dplyr::select(-ratio, -change) %>%
        dplyr::filter(relative_change != 0) %>%
        tidyr::pivot_wider(
            names_from = change_sign,
            values_from = value) %>%
        dplyr::mutate(relative_value_loss_vs_gain = `-1`/ `1`) %>%
        dplyr::select(-dplyr::matches("1")) %>%
        ggplot(aes(x = relative_change, y = relative_value_loss_vs_gain, group = w, col = w, lty = w)) +
        geom_line() +
        ggplot2::scale_color_discrete("Internal noise (w)", drop = FALSE) +
        ggplot2::scale_linetype_discrete("Internal noise (w)", drop = FALSE) +
        labs(
            x = "Relative change (relative to endowment)",
            y = TeX("Loss aversion $\\left(\\frac{\\left|Utility_{Loss}\\right|}{\\left|Utility{Gain}\\right|}\\right)$")
        )
}

add_tk_data <- function(dat = .){
    dat %>%
        # Add Tversky and Kahneman (1992) model
        dplyr::mutate(
            value_tk = abs(relative_change)^.88,
            value_tk = dplyr::if_else(
                relative_change < 0,
                -2.25 * value_tk,
                value_tk
            ),
            value_tk_scaled = value_tk * max(value) / max(value_tk)
        )
}
