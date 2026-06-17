generate_dummy_data <- function(p.neutral.low.ratio,
                                p.neutral.high.ratio,
                                rr.low.ratio,
                                rr.high.ratio,
                                n_subj = 100,
                                n_trials = 12,
                                return_aggregate = FALSE,
                                return_diff_scores = FALSE) {
    if (return_diff_scores) {
        # We need to calculate averages to calculate difference scores
        return_aggregate <- TRUE
    }

    # Create the dummy data
    dummy_data <- expand.grid(
        subject = 1:n_subj,
        ratio.range = c("pre-asymptotic", "asymptotic"),
        vivacity = c("neutral", "vivid"),
        trial = 1:n_trials
    ) %>%
        dplyr::mutate(
            # Create the base probabilities for each combination of ratio.range and vivacity
            prob = dplyr::case_when(
                ratio.range == "pre-asymptotic" & vivacity == "neutral" ~ p.neutral.low.ratio,
                ratio.range == "pre-asymptotic" &
                    vivacity == "vivid" ~ p.neutral.low.ratio / rr.low.ratio,
                ratio.range == "asymptotic" &
                    vivacity == "neutral" ~ p.neutral.high.ratio,
                ratio.range == "asymptotic" &
                    vivacity == "vivid" ~ p.neutral.high.ratio / rr.high.ratio
            ),

            # Generate binary outcomes based on probabilities
            outcome = rbinom(dplyr::n(), size = 1, prob = prob)
        ) %>%
        dplyr::select(-prob)

    if (return_aggregate) {
        dummy_data <- dummy_data %>%
            dplyr::group_by(subject, ratio.range, vivacity) %>%
            dplyr::summarize(outcome = mean(outcome, na.rm = TRUE), .groups = "drop")
    }

    if (return_diff_scores) {
        dummy_data <- dummy_data %>%
            tidyr::pivot_wider(
                id_cols = c("subject", "ratio.range"),
                names_from = "vivacity",
                values_from = "outcome"
            ) %>%
            dplyr::mutate(
                d.absolute = neutral - vivid,
                d.relative = 7 / 5 * (neutral - vivid) / (neutral + vivid)
            ) %>%
            tidyr::pivot_longer(c("d.absolute", "d.relative"),
                                names_to = "difference.type",
                                values_to = "d"
            ) %>%
            # Arranging is needed for the wilcoxon test we will do below
            dplyr::arrange(difference.type, ratio.range, subject)
    }

    return(dummy_data)
}

simulate_and_check_significance_ia <- function(p.neutral.low.ratio,
                                               p.neutral.high.ratio,
                                               rr.low.ratio,
                                               rr.high.ratio,
                                               n_subj = 100,
                                               n_trials = 12,
                                               ...) {
    # Create the dummy data
    dummy_data <- generate_dummy_data(
        p.neutral.low.ratio,
        p.neutral.high.ratio,
        rr.low.ratio,
        rr.high.ratio,
        n_subj,
        n_trials
    )

    # Fit the GLMM model
    model <- glmer(
        outcome ~ ratio.range * vivacity + (1 | subject),
        data = dummy_data,
        family = binomial,
        control = glmerControl(optimizer = "bobyqa")
    )

    # Extract summary and filter for interaction term significance
    ia_signif <- broom.mixed::tidy(model) %>%
        dplyr::filter(stringr::str_detect(term, "ratio.range.*vivacity")) %>%
        dplyr::mutate(ia_signif = (p.value < .05) * 1) %>%
        dplyr::pull(ia_signif)

    return(ia_signif)
}

# Run the simulations multiple times based on a data frame of conditions
simulate_and_check_significance_wrapper <- function(dat.conds = .,
                                                    n_sim = 1000,
                                                    verbose = TRUE,
                                                    n_subj = 100,
                                                    n_trials = 12,
                                                    fun = simulate_and_check_significance_ia,
                                                    difference_type = NULL) {
    dat.conds %>%
        dplyr::group_by(
            experimentID,
            p.neutral.low.ratio,
            p.neutral.high.ratio,
            rr.low.ratio,
            rr.high.ratio
        ) %>%
        dplyr::group_modify(~ {
            # Print user message
            if (verbose) {
                .x %>%
                    tidyr::unite(conds, dplyr::everything(), sep = " ") %>%
                    dplyr::mutate(conds = stringr::str_c("Processing values ", conds)) %>%
                    dplyr::pull(conds) %>%
                    message()
            }

            # Calculate
            data.frame(ia_signif = furrr::future_map_dbl(1:n_sim, function(i) {
                .x %>%
                    dplyr::mutate(
                        ia_signif =
                            fun(
                                p.neutral.low.ratio = p.neutral.low.ratio,
                                p.neutral.high.ratio = p.neutral.high.ratio,
                                rr.low.ratio = rr.low.ratio,
                                rr.high.ratio = rr.high.ratio,
                                n_subj = n_subj,
                                n_trials = n_trials,
                                difference_type = difference_type
                            )
                    ) %>%
                    dplyr::pull(ia_signif)
            }) %>%
                mean())
        }, .keep = TRUE) %>%
        dplyr::ungroup()
}

describe_ratio_range_params <- function(specs_df) {
    param_labels <- c(
        "p.neutral.low.ratio" = "the acceptability probability in the neutral condition in the pre-asymptotic range",
        "p.neutral.high.ratio" = "the acceptability probability in the neutral condition in the asymptotic range",
        "rr.low.ratio" = "the risk ratio of this probability between the neutral and vivid conditions in the pre-asymptotic range",
        "rr.high.ratio" = "the risk ratio of this probability between the neutral and vivid conditions in the asymptotic range"
    )

    specs_df %>%
        dplyr::mutate(
            description = glue::glue("{param_labels[param]} ranged from {min} to {max} in steps of {step}")
        ) %>%
        dplyr::pull(description) %>%
        stringr::str_c(collapse = "; ")
}

simulate_and_check_significance_diff_score <- function(p.neutral.low.ratio,
                                                       p.neutral.high.ratio,
                                                       rr.low.ratio,
                                                       rr.high.ratio,
                                                       n_subj = 100,
                                                       n_trials = 12,
                                                       difference_type = "d.relative",
                                                       ...) {
    # Create the dummy data
    dummy_data <- generate_dummy_data(p.neutral.low.ratio,
                                      p.neutral.high.ratio,
                                      rr.low.ratio,
                                      rr.high.ratio,
                                      n_subj,
                                      n_trials,
                                      return_diff_scores = TRUE
    )

    # Check wilcoxon significance
    # the variable name is for consistency with the wrapper function
    ia_signif <- dummy_data %>%
        rstatix::df_nest_by(difference.type) %>%
        dplyr::mutate(tmp = purrr::map(
            data,
            ~ .x %>%
                # Sort once more for safety
                dplyr::arrange(ratio.range, subject) %>%
                rstatix::wilcox_test(d ~ ratio.range, paired = TRUE, detailed = FALSE)
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(tmp) %>%
        dplyr::mutate(ia_signif = (p < .05) * 1) %>%
        dplyr::filter(difference.type == difference_type) %>%
        dplyr::pull(ia_signif)

    return(ia_signif)
}
