get.ratio.before.asymptote <- function(dat = ., search.strategy = c("largest.different.from.rightmost.ratio", "smallest.not.different.from.rightmost.ratio"), test = c("wilcoxon", "mcnemar", "glmm"), verbose = TRUE) {
    search.strategy <- match.arg(search.strategy)
    test <- match.arg(test)

    if (test == "wilcoxon") {
        # If we use the wilcoxon test, we need continuous data, so we use the raw ratings
        # rather than the binary version
        # We then simply average the ratings

        dat.m <- dat %>%
            dplyr::group_by(ResponseId, ratio) %>%
            dplyr::summarize(rating = mean(rating.raw, na.rm = TRUE), .groups = "drop")
    } else {
        if (test == "mcnemar") {
            dat.m <- dat %>%
                dplyr::mutate(rating = factor(rating.bin,
                                              levels = c(1, 0),
                                              labels = c("acceptable", "unacceptable")
                ))
        } else {
            stop("test not implemented")
        }
    }

    if (verbose) {
        message(glue::glue("We use a ", test, " test, using the search strategy ", search.strategy))
    }

    # This is the test function we use below
    criterion.test.fun <- function(dat.inner = ., test) {
        dat.out <- NA_integer_

        if (test == "wilcoxon") {
            safe_wilcox_test <- purrr::possibly(rstatix::wilcox_test, otherwise = NULL)
            dat.out <- dat.inner %>%
                #  paired = TRUE expects the order of each observation to be the same in each group.
                dplyr::arrange(ratio, ResponseId) %>%
                safe_wilcox_test(rating ~ ratio, paired = TRUE)
        }

        if (test == "mcnemar") {
            safe_mcnemar <- purrr::possibly(rstatix::pairwise_mcnemar_test, otherwise = NULL)

            dat.out <- dat.inner %>%
                #  paired = TRUE expects the order of each observation to be the same in each group.
                dplyr::arrange(ratio, ResponseId) %>%
                safe_mcnemar(rating ~ ratio | ResponseId)

            if (!is.null(dat.out)) {
                if (nrow(dat.out) > 1) {
                    stop("Incorrect number of rows, ratios are ", paste0(unique(dat.inner$ratio), collapse = ";"))
                }
            }
        }

        if (is.data.frame(dat.out) | is.null(dat.out)) {
            return(dat.out)
        } else {
            stop(glue::glue("Test ", test, " is not implemented."))
        }
    }

    # Main function starts here

    # Vector of ratios
    v.ratio <- sort(unique(dat.m$ratio), decreasing = TRUE)

    if (verbose) {
        message(glue::glue("Detected ratios ", knitr::combine_words(v.ratio)))
    }

    # Compare acceptabilities for all ratios to that for the largest ratio
    l_test_results <- purrr::map(
        v.ratio[-1],
        ~ dat.m %>%
            dplyr::filter(
                (ratio == v.ratio[1]) |
                    (ratio == .x)
            ) %>%
            criterion.test.fun(test = test)
    ) %>%
        # Remove NULLs
        purrr::compact()

    if (length(l_test_results) == 0) {
        critical_ratio <- NA_real_
        warning("All test results were NULL; setting critical_ratio to NA")
        warning("Input size was: ", paste(dim(dat), collapse = ";"))
    } else {
        # We have some results and try to find the critical ratio

        l_test_results <- l_test_results %>%
            purrr::list_rbind(names_to = NULL) %>%
            dplyr::select(group1, group2, p) %>%
            dplyr::rename(
                ratio.comp = group1,
                ratio.max = group2
            )

        if (search.strategy == "largest.different.from.rightmost.ratio") {
            # Find LARGEST ratio for which the ratings are
            # significantly different from the rightmost ratio in the design

            l_test_results <- l_test_results %>%
                dplyr::filter(p <= .05)

            if (nrow(l_test_results) == 0) {
                warning("There were no ratios where acceptability was different from the largest ratio, setting critical_ratio to NA")

                critical_ratio <- NA_real_
            } else {
                critical_ratio <- l_test_results %>%
                    dplyr::pull(ratio.comp) %>%
                    max(na.rm = TRUE)
            }
        } else {
            if (search.strategy == "smallest.not.different.from.rightmost.ratio") {
                # Find SMALLEST ratio for which the ratings are
                # NOT significantly different from the rightmost ratio in the design

                l_test_results <- l_test_results %>%
                    dplyr::filter(p > .05)

                if (nrow(l_test_results) == 0) {
                    warning("There were no ratios where acceptability was NOT different from the largest ratio, setting critical_ratio to NA")

                    critical_ratio <- NA_real_
                } else {
                    critical_ratio <- l_test_results %>%
                        dplyr::pull(ratio.comp) %>%
                        min(na.rm = TRUE)
                }
            } else {
                stop("Invalid search strategy")
            }
        } # else from if(search.strategy == "largest.different.from.rightmost.ratio")
    } # else from if(length(l_test_results) == 0)

    # We have found the critical ratio (potentially NA_real_) and now make some verifications

    # Verify that ratings for the critical ratio are greater than for the smallest ratio in the design

    if (!is.na(critical_ratio) & !is.null(critical_ratio)) {
        p.smallest.ratio.vs.critical.ratio <- dat.m %>%
            dplyr::filter(
                (ratio == critical_ratio) |
                    (ratio == min(v.ratio))
            ) %>%
            criterion.test.fun(test = test)

        if (is.null(p.smallest.ratio.vs.critical.ratio)) {
            critical_ratio <- NA_real_

            warning(glue::glue(
                "Test of tatings at critical ratio against those from ",
                "the smallest ratio failed, setting critical ratio to NA"
            ))
        } else {
            if (nrow(p.smallest.ratio.vs.critical.ratio) == 0) {
                critical_ratio <- NA_real_

                warning(glue::glue(
                    "Test of tatings at critical ratio against those from ",
                    "the smallest ratio failed, setting critical ratio to NA"
                ))
            } else {
                # p.smallest.ratio.vs.critical.ratio should be a data frame here. There is a bug if it isn't

                if (!is.data.frame(p.smallest.ratio.vs.critical.ratio)) {
                    stop("p.smallest.ratio.vs.critical.ratio should be a data frame. There is some weird bug or weird data.")
                }

                p.smallest.ratio.vs.critical.ratio <- p.smallest.ratio.vs.critical.ratio %>%
                    dplyr::pull(p)

                # p can still be NA
                if (!is.numeric(p.smallest.ratio.vs.critical.ratio) |
                    is.na(p.smallest.ratio.vs.critical.ratio)) {
                    warning(glue::glue("p.smallest.ratio.vs.critical.ratio is not numeric or NA:, ", p.smallest.ratio.vs.critical.ratio))
                } else {
                    if (p.smallest.ratio.vs.critical.ratio > .05) {
                        critical_ratio <- NA_real_
                        warning(glue::glue(
                            "Ratings at critical ratio are not different",
                            "from those for the smallest ratio, setting critical ratio to NA"
                        ))
                    }

                    # Otherwise we accept the critical_ratio
                }
            } # else from if(!is.na(critical_ratio) & !is.null(critical_ratio)){
        } # else from if(is.null(p.smallest.ratio.vs.critical.ratio)){

        if (verbose) {
            message(glue::glue(
                "Ratios below ", critical_ratio, " are in the pre-asymptotic range;",
                " ratios above are in the asymptotic range"
            ))
        }
    } else {
        warning("No critical ratios has been found, return NA")

        critical_ratio <- NA_real_
    }

    return(as.numeric(critical_ratio))
}

add_experimentID_to_cond_find_asympt <- function(v.exps, dat_cond) {
    purrr::map_dfr(
        v.exps,
        ~ dat_cond %>%
            dplyr::mutate(experimentID = .x)
    )
}

# identify critical ratio in a single bootstrap sample
get.ratio.before.asymptote.bootstrap <- function(dat = ., dat_cond, verbose = TRUE) {
    dat_cond %>%
        dplyr::mutate(
            critical.ratio =
                purrr::pmap_dbl(
                    list(
                        search.strategy = search.strategy,
                        test = test,
                        experimentID = experimentID,
                        blocks = blocks
                    ),
                    ~ dat %>%
                        dplyr::filter(
                            experimentID == ..3,
                            blocks == ..4
                        ) %>%
                        get.ratio.before.asymptote(search.strategy = ..1, test = ..2, verbose = verbose)
                )
        )
}

# wrapper to run bootstrap fits
run_bootstrap_fit_for_asymptote <- function(dat, v.exps,
                                            dat_cond,
                                            subj_col = ResponseId,
                                            n_samples = N_BOOTSTRAP,
                                            bootstrap_batch_size = BOOTSTRAP_BATCH_SIZE) {
    subj_col <- rlang::enquo(subj_col)

    gc() # Clean up memory just in case

    # Create bootstrap samples
    l.bootstrap.samples <- dat %>%
        dplyr::filter(
            question == "acceptability",
            vivacity == "vivid",
            experimentID %in% v.exps
        ) %>%
        generate_bootstrap_samples(subj_col = !!subj_col, n_samples = n_samples, reindex_subj_col = TRUE) %>%
        split_by_length(bootstrap_batch_size)

    gc() # Clean up memory just in case

    # Run fits for the bootstrap samples
    dat.bootstrap.fits <- purrr::map_dfr(
        seq_along(l.bootstrap.samples),
        function(i) {
            message(
                "Processing batch ", i, " of ",
                length(l.bootstrap.samples),
                " (boostrap fits of asymptotes in Experiments ", paste0(v.exps, collapse = " and "), ")"
            )

            dat_batch_result <- furrr::future_imap_dfr(
                l.bootstrap.samples[[i]],
                ~ {
                    get.ratio.before.asymptote.bootstrap(.x,
                                                         add_experimentID_to_cond_find_asympt(
                                                             v.exps,
                                                             dat_cond
                                                         ),
                                                         verbose = !RECALCULATE_EVERYTHING_APP
                    )
                },
                .id = "bootstrap_sample",
                furrr_options(
                    seed = TRUE,
                    stdout = !RECALCULATE_EVERYTHING_APP
                )
            ) %>%
                dplyr::mutate(
                    bootstrap_sample =
                        as.integer(bootstrap_sample) + (i - 1) * bootstrap_batch_size
                )

            gc()

            return(dat_batch_result)
        }
    )

    return(dat.bootstrap.fits)
}
