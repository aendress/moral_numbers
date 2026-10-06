## ----deal-with-environment, echo = FALSE, include = FALSE, eval = FALSE--------------------------
# # https://rstudio.github.io/renv/
# # renv::init(project = "../")
# # renv::snapshot()
# # renv::restore()


## ----extract-code, eval = FALSE, echo = FALSE, include = FALSE-----------------------------------
# # Extract code to run on the command line, which is much faster than in the GUI
# 
# knitr::purl(
#     here::here(
#         "code",
#         "moral_numbers_tmp.Rmd"
#     ),
#     output = here::here(
#         "code",
#         "moral_numbers_extracted.R"
#     )
# )
# 
# lintr::lint(
#     here::here(
#         "code",
#         "moral_numbers_extracted.R"
#         )
# )


## ----check-code, eval = FALSE, echo = FALSE, include = FALSE-------------------------------------
# lintr::lint("moral_numbers.Rmd")


## ----setup, echo = FALSE, include = FALSE--------------------------------------------------------
rm(list = ls())

# use bootstrap fits in figures and simplify code

options(
    digits = 3,
    knitr.kable.NA = ""
)
knitr::opts_chunk$set(
    # Run the chunk
    eval = TRUE,
    # Don't include source code
    echo = FALSE,
    # Print warnings to console rather than the output file
    warning = FALSE,
    # Stop on errors
    error = FALSE,
    # Print message to console rather than the output file
    message = FALSE,
    # Include chunk output into output
    include = TRUE,
    # Don't reformat R code
    tidy = FALSE,
    # Center images
    fig.align = "center",
    # Default image width
    out.width = "80%"
)

# other knits options are here:
# https://yihui.name/knitr/options/

# Use latex, not pandoc to avoid spurious longtable output
knitr::opts_knit$set(kable.force.latex = TRUE)


## ----set-parameters, echo = FALSE, include=FALSE-------------------------------------------------
DATA_DIR <- here::here("data")
OUTPUT_DIR <- here::here("code", "output")
EXPERIMENT_DIR <- here::here("experiments")
HELPER_DIR <- here::here("code", "helper_functions")
CHILD_RMD_DIR <- here::here("code", "child_rmds")

REMOVE_INCOMPLETE_SUBJ <- TRUE
REMOVE_CONSTANT_SUBJ <- TRUE
REMOVE_FAST_SUBJ <- TRUE

# Recalculate even the time consuming stuff
# If FALSE, time consuming things like bootstrap fits are read from a file
RECALCULATE_EVERYTHING <- TRUE

# Recalculate things in the appendix. Might be false (e.g., for the power analysis) even if we want to recalculate stuff in the main text
RECALCULATE_EVERYTHING_APP <- TRUE

# Optionally reload data, since this takes ages
RELOAD_DATA <- TRUE


N_BOOTSTRAP <- 10000
# To reduce memory problems, only used for fits of the asympotes
BOOTSTRAP_BATCH_SIZE <- 100

FIT_PARAMS <- list(
    start = list(w = .6, a = 1),
    lower = c(w = .01, a = 1),
    # This was choosing as the unconstraint a's were well below 2
    upper = c(w = 2, a = 5)
)

# Should the fits in the figures be based on the fits for the overall means or the boostrap fit?
# FIT_FOR_FIGURE <- "linear"
# FIT_FOR_FIGURE <- "overall"
FIT_FOR_FIGURE <- "bootstrap"

# Plot individual plots or just those that go into the paper?
SHOW_INDIVIDUAL_PLOTS <- FALSE

# Print presentation-ready figures (black background, large fonts)
PRINT_PRESENTATION_FIGURES <- TRUE

# Set seed to Cesar's birthday
set.seed(1207100)


## ----define-trial-structure----------------------------------------------------------------------
# TRIALS_INFO: block structure for every experiment.
#
# Columns:
#   experimentID          – internal experiment identifier
#   n_scenarios_per_block – unique scenario-decision combinations per block
#   n_blocks              – number of within-subjects blocks
#   n_questions           – rated dimensions per scenario (used only to derive
#                           TRIALS_PER_EXP; NOT shown in the output table)
#   block_type            – human-readable label for the block contrast,
#                           or NA for single-block experiments
#
# Total trials per participant = n_blocks * n_scenarios_per_block * n_questions
# TRIALS_PER_EXP is derived from this tibble below.

TRIALS_INFO <- dplyr::bind_rows(list(
    
    # ── Experiment 1 ──────────────────────────────────────────────────────────
    # Basic trolley-problem acceptability task, single block.
    exp1 = list(
        experimentID          = "exp1",
        n_scenarios_per_block = 12L,
        n_blocks              = 1L,
        n_questions           = 1L,   # acceptability only
        block_type            = NA_character_
    ),
    
    # Replication of Exp. 1 with the first scenario and small-number conditions
    # removed per the pre-registration.
    exp1b = list(
        experimentID          = "exp1b",
        n_scenarios_per_block = 10L,
        n_blocks              = 1L,
        n_questions           = 1L,
        block_type            = NA_character_
    ),
    
    # ── Experiments 2a–d: ratio vs. utility / harm ────────────────────────────
    # Two-block (moral / economic) design; 1 acceptability question per scenario.
    exp2a = list(
        experimentID          = "exp2a",
        n_scenarios_per_block = 12L,
        n_blocks              = 2L,
        n_questions           = 1L,
        block_type            = "moral / economic"
    ),
    exp2b = list(
        experimentID          = "exp2b",
        n_scenarios_per_block = 12L,
        n_blocks              = 2L,
        n_questions           = 1L,
        block_type            = "moral / economic"
    ),
    # Replication of 2a run in the same Qualtrics file.
    exp2c = list(
        experimentID          = "exp2c",
        n_scenarios_per_block = 12L,
        n_blocks              = 2L,
        n_questions           = 1L,
        block_type            = "moral / economic"
    ),
    # Replication of 2a/2c with a less extreme ratio contrast.
    exp2d = list(
        experimentID          = "exp2d",
        n_scenarios_per_block = 12L,
        n_blocks              = 2L,
        n_questions           = 1L,
        block_type            = "moral / economic"
    ),
    
    # ── Experiments 3, 3b–d: "evil choice" and in-group variants ─────────────
    # Single-block variants run within the same Qualtrics files as Experiments
    # 2a–d and 4. Participants saw only evil-choice or in-group scenarios (no
    # economic block), hence 1 block and 1 question.
    exp3 = list(
        experimentID          = "exp3",
        n_scenarios_per_block = 12L,
        n_blocks              = 1L,
        n_questions           = 1L,
        block_type            = NA_character_
    ),
    exp3b = list(
        experimentID          = "exp3b",
        n_scenarios_per_block = 12L,
        n_blocks              = 1L,
        n_questions           = 1L,
        block_type            = NA_character_
    ),
    exp3c = list(
        experimentID          = "exp3c",
        n_scenarios_per_block = 12L,
        n_blocks              = 1L,
        n_questions           = 1L,
        block_type            = NA_character_
    ),
    exp3d = list(
        experimentID          = "exp3d",
        n_scenarios_per_block = 12L,
        n_blocks              = 1L,
        n_questions           = 1L,
        block_type            = NA_character_
    ),
    
    # ── Experiment 4 (paper: Experiment 2) ───────────────────────────────────
    # Replication of Exp. 1b with a moral / economic block design but fewer
    # scenarios (10 rather than 12).
    exp4 = list(
        experimentID          = "exp4",
        n_scenarios_per_block = 10L,
        n_blocks              = 2L,
        n_questions           = 1L,
        block_type            = "moral / economic"
    ),
    
    # ── Experiments 5, 5b, 6: failed vivacity manipulations ──────────────────
    # Attempts to increase victim salience via vivid descriptions; all failed
    # because victims and beneficiaries died from the same causes.
    # 14 scenarios per block; only acceptability was rated.
    exp5 = list(
        experimentID          = "exp5",
        n_scenarios_per_block = 14L,
        n_blocks              = 2L,
        n_questions           = 1L,
        block_type            = "vivid / neutral"
    ),
    # Pilot variant of Exp. 5 with only the vivid block.
    exp5b = list(
        experimentID          = "exp5b",
        n_scenarios_per_block = 14L,
        n_blocks              = 1L,
        n_questions           = 1L,
        block_type            = NA_character_
    ),
    exp6 = list(
        experimentID          = "exp6",
        n_scenarios_per_block = 14L,
        n_blocks              = 2L,
        n_questions           = 1L,
        block_type            = "vivid / neutral"
    ),
    
    # ── Experiment 7: failed vivacity with incommensurate deaths ─────────────
    # Vivid vs. neutral; 3 questions per scenario: 1 acceptability + 2 severity
    # ratings (victims' deaths and beneficiaries' deaths separately).
    # 2 blocks × 12 scenarios × 3 questions = 72.
    exp7 = list(
        experimentID          = "exp7",
        n_scenarios_per_block = 12L,
        n_blocks              = 2L,
        n_questions           = 3L,
        block_type            = "vivid / neutral"
    ),
    
    # ── Experiments 8, 9a, 10, 11: successful vivacity manipulations ──────────
    # Vivid vs. neutral; 2 questions per scenario (acceptability + severity).
    
    # Exp. 8 (paper: Experiment 4): 2 × 12 × 2 = 48.
    exp8 = list(
        experimentID          = "exp8",
        n_scenarios_per_block = 12L,
        n_blocks              = 2L,
        n_questions           = 2L,   # acceptability + severity
        block_type            = "vivid / neutral"
    ),
    
    # Exp. 9a (paper: Experiment AS): asymptote-search pilot — vivid condition
    # only, with more extreme ratios; 10 scenarios rather than 12.
    # 1 × 10 × 2 = 20.
    exp9a = list(
        experimentID          = "exp9a",
        n_scenarios_per_block = 10L,
        n_blocks              = 1L,
        n_questions           = 2L,
        block_type            = NA_character_   # vivid condition only
    ),
    
    # Exp. 10 (paper: Experiment 5): 2 × 12 × 2 = 48.
    exp10 = list(
        experimentID          = "exp10",
        n_scenarios_per_block = 12L,
        n_blocks              = 2L,
        n_questions           = 2L,
        block_type            = "vivid / neutral"
    ),
    
    # Exp. 11 (paper: Experiment 6): replication of Exp. 10. 2 × 12 × 2 = 48.
    exp11 = list(
        experimentID          = "exp11",
        n_scenarios_per_block = 12L,
        n_blocks              = 2L,
        n_questions           = 2L,
        block_type            = "vivid / neutral"
    ),
    
    # ── Experiment 99 (Briana) ────────────────────────────────────────────────
    # Adjacent project; structure not fully integrated into this pipeline.
    exp99 = list(
        experimentID          = "exp99",
        n_scenarios_per_block = NA_integer_,
        n_blocks              = NA_integer_,
        n_questions           = NA_integer_,
        block_type            = NA_character_
    )
))

# Derive TRIALS_PER_EXP from TRIALS_INFO so both are always consistent.
TRIALS_PER_EXP <- TRIALS_INFO |>
    dplyr::mutate(n_trials = n_blocks * n_scenarios_per_block * n_questions) |>
    dplyr::select(experimentID, n_trials) |>
    tibble::deframe()




## ----verify-trial-per-exp, eval = FALSE----------------------------------------------------------
# dplyr::left_join(
#     # This is the automatic calculation
#     dat.moral.numbers %>%
#         dplyr::filter(question == "acceptability") %>%
#         dplyr::count(experimentID, ResponseId) %>%
#         dplyr::distinct(experimentID, n),
# 
#     # This is the manual calculation
#     TRIALS_PER_EXP %>%
#         tibble::enframe("experimentID", "n_manual"),
#     by = "experimentID"
# ) %>%
#     dplyr::filter(n != n_manual)


## ----load-libraries, include = FALSE, message = TRUE, warning = TRUE-----------------------------
if (Sys.info()[["user"]] %in% c("ansgar", "endress")) {
    source("/Users/endress/R.ansgar/ansgarlib/R/tt.R")
    source("/Users/endress/R.ansgar/ansgarlib/R/null.R")
    # source ("helper_functions.R")
} else {
    # Note that these will probably not be the latest versions
    source("http://endress.org/progs/tt.R")
    source("http://endress.org/progs/null.R")
}

librarian::shelf(
    tidyverse,
    knitr,
    lme4,
    here,
    latex2exp,
    bookdown,
    lintr,
    glue,
    kableExtra,
    # gridExtra,
    mgsub, # For mgsub::mgsub() in rename.terms() in helper_functions/moral_numbers_general.R
    tidytext, # For reorder_within
    cowplot,
    ggpubr,
    ggthemes,
    RColorBrewer,
    xlsx,
    MuMIn, # For AICc
    broom,
    broom.mixed,
    # tictoc,
    future,
    "furrr", # Parallel version of purrr
    # forcats, # part of tidyverse
    XML,
    xml2,
    janitor,
    # assertthat, # could replace e.g. if (length(mu) != length(sigma)) stop("...") with assert_that(length(mu) == length(sigma))
    # Hmisc,
    scales,
    # bootstrap,
    rsample,
    caret,
    purrr,
    # rJava,
    rcompanion,
    rlang,
    rstatix,
    stringr,
    minpack.lm, # for nlsLM
    pwr, # for pwr.t.test
    
    ggalt, # for geom_spikelines
    ggh4x # for coord_axes_inside
    # renv,
    # lmtest,
    # performance,
    # nonnest2,
    # data.table
    # patchwork # See https://ggplot2-book.org/arranging-plots.html
)

# Source helper files
# list.files(
#     path = HELPER_DIR,
#     pattern = "\\.[Rr]$",
#     full.names = TRUE
# ) %>%
#     purrr::walk(source)

load_helper_files(HELPER_DIR) 

# Add environment
# https://rstudio.github.io/renv/

future::plan(multisession, workers = 4)
# future::plan(multicore, workers = future::availableCores() - 1)

# Suppress summarise info
options(dplyr.summarise.inform = FALSE)

# Set default theme
# ggplot2::theme_set(theme_linedraw(14))
# ggplot2::theme_set(cowplot::theme_minimal_grid(14))
ggplot2::theme_set(ggthemes::theme_clean(14))
theme_update(
    legend.position = "bottom",
    legend.justification = "center",
    strip.background = ggplot2::element_blank(),
    strip.text = ggplot2::element_text(face = "bold")
)

# # Palettes form https://www.datanovia.com/en/blog/top-r-color-palettes-to-know-for-great-data-visualization/
# # display.brewer.all(colorblindFriendly = TRUE)
# #display.brewer.pal(n = 3, name = 'Set2')
# v.violin.palette <- RColorBrewer::brewer.pal(n = 3, name = "Set2")

set_parent_mode()


## ----load-data-question-labels-define-files, warning = FALSE-------------------------------------
# Define Qualtrics files and suffix/filters
dat_moral_numbers_question_labels_specs <- list(
    list(
        label = "exp2a3",
        file = "Moral_Numbers_2a-MoralEconomic_choices-ratios_vs_utility__evil_choice.qsf",
        filter.str = NA_character_,
        add_suffix_1 = FALSE
    ),
    list(
        label = "exp2b3",
        file = "Moral_Numbers_2b-MoralEconomic_choices-ratios_vs_number_of_victims__evil_choice.qsf",
        filter.str = NA_character_,
        add_suffix_1 = FALSE
    ),
    list(
        label = "exp2c3",
        file = "Moral_Numbers_2c-MoralEconomic_choices-ratios_vs_utility__evil_choice_-_Replication.qsf",
        filter.str = NA_character_,
        add_suffix_1 = FALSE
    ),
    list(
        label = "exp2d3b",
        file = "Moral_Numbers_2d3b-MoralEconomic_choices-ratios_vs_utility_replication_with_less_extreme_contrast.qsf",
        filter.str = NA_character_,
        add_suffix_1 = FALSE
    ),
    list(
        label = "exp43d",
        file = "Moral_Numbers_4_-_Exp_1b_with_moral_vs_economic_choices__Exp_3d_EvilIngroup_favoritism.qsf",
        filter.str = NA_character_,
        add_suffix_1 = FALSE
    ),
    list(
        label = "exp53d",
        file = "Moral_Numbers_5_-_vivid_and_neutral_moral_choices__Exp_3d_EvilIngroup_favoritism.qsf",
        filter.str = NA_character_,
        add_suffix_1 = FALSE
    ),
    list(
        label = "exp5b",
        file = "Moral_Numbers_5b_-_vivid_and_neutral_moral_choices_-_1_block_only_-_different_ratios.qsf",
        filter.str = NA_character_,
        add_suffix_1 = FALSE
    ),
    list(
        label = "exp6",
        file = "Moral_Numbers_6_-_vivid_and_neutral_moral_choices.qsf",
        filter.str = NA_character_,
        add_suffix_1 = FALSE
    ),
    list(
        label = "exp7.no.subQuestions",
        file = "Moral_Numbers_Exp_7_-_vivid_vs_neutral_with_incomensurate_scenarios.qsf",
        filter.str = "scenario|attentionCheckActual",
        add_suffix_1 = FALSE
    ),
    list(
        label = "exp8",
        file = "Moral_Numbers_Exp_8_-_vivid_vs_neutral_with_incomensurate_scenarios_-_attention_check_corrected.qsf",
        filter.str = "scenario|attChk",
        add_suffix_1 = TRUE
    ),
    list(
        label = "exp9a",
        file = "Moral_Numbers_Exp_9a_-_verifying_asymptote_in_neutral_condition.qsf",
        filter.str = "scenario|attChk",
        add_suffix_1 = TRUE
    ),
    list(
        label = "exp10",
        file = "Moral_Numbers_Exp_10_-_vivid_vs_neutral_with_incomensurate_scenarios_higher_ratios.qsf",
        filter.str = "scenario|attChk",
        add_suffix_1 = TRUE
    ),
    list(
        label = "exp10.corrected",
        file = "Moral_Numbers_Exp_10_-_vivid_vs_neutral_with_incomensurate_scenarios_higher_ratios-_corrected.qsf",
        filter.str = "scenario|attChk",
        add_suffix_1 = TRUE
    ),
    list(
        label = "exp10.corrected.cb1.only",
        file = "Moral_Numbers_Exp_10_-_vivid_vs_neutral_with_incomensurate_scenarios_higher_ratios-_correct_cb1.qsf",
        filter.str = "scenario|attChk",
        add_suffix_1 = TRUE
    ),
    list(
        label = "exp11",
        file = "exp11_replication_of_exp10_vivid_vs_neutral.qsf",
        filter.str = "scenario|attChk",
        add_suffix_1 = TRUE
    )
) %>% 
    tibble::enframe(name = NULL, value = "data") %>% 
    tidyr::unnest_wider(data)




## ----load-data-question-labels-load-files, warning = FALSE---------------------------------------
if (RELOAD_DATA) {
    # now loop through these files and extract the labels
    l_moral_numbers_question_labels <- dat_moral_numbers_question_labels_specs %>%
        purrr::pmap(\(label, file, filter.str, add_suffix_1){
            path <- here::here(
                DATA_DIR,
                "qualtrics_qsf_files", 
                file
            )
            filter_str <- if (is.na(filter.str)) "scenario" else filter.str
            
            labels <- get.question.labels.from.qsf(path,
                                                   # The filter string below ALLOWS matches to go through
                                                   filter.str = filter_str
            )
            
            # For some reason all question IDs finish with _1 in some versions of qualtrics
            # Relabel the questions
            # Paste messes up the names, so use []
            if (isTRUE(add_suffix_1)) labels[] <- paste0(labels, "_1")
            
            labels
        }) %>%
        purrr::set_names(dat_moral_numbers_question_labels_specs$label)
}


## ----load-number-conditions----------------------------------------------------------------------
if (RELOAD_DATA) {
    dat.number.conds <-
        # Define paths and other information for number conditions
        list(
            list(
                relative_path = "experiment1/number_conds.csv", 
                experimentID = "exp1", 
                experimentDesc = "Experiment 1a", 
                Group = NA_character_, 
                rename_victims = FALSE
            ),
            
            list(
                relative_path = "experiment1b/number_conds_replication.csv",
                experimentID = "exp1b",
                experimentDesc = "Experiment 1b", 
                Group = NA_character_,
                rename_victims = FALSE
            ),
            list(
                relative_path = "experiment2_3/final/number_conditions/number_conds_2x2_choices_ratio_vs_utility.csv", 
                experimentID = "exp2ac", 
                experimentDesc = "Experiment 2a and 2c (replication): Pitting the ratio against the utility: ", 
                Group = NA_character_, 
                rename_victims = TRUE),
            list(
                relative_path = "experiment2_3/final/number_conditions/number_conds_2x2_choices_ratio_vs_utility_1.5utility_3xvictims.csv", 
                experimentID = "exp2d", 
                experimentDesc = "Experiment 2d: Replication of Experiment 2a/2c with less extreme contrasts", 
                Group = NA_character_, 
                rename_victims = TRUE
            ),
            list(
                relative_path = "experiment2_3/final/number_conditions/number_conds_2x2_choices_ratio_vs_victims.csv", 
                experimentID = "exp2b", 
                experimentDesc = "Experiment 2b: Pitting the ratio against the number of victims", 
                Group = NA_character_, 
                rename_victims = TRUE
            ),
            list(
                relative_path = "experiment4/number_conds_replication.csv", 
                experimentID = "exp4", 
                experimentDesc = "Experiment 4", 
                Group = NA_character_, 
                rename_victims = FALSE
            ),
            list(
                relative_path = "experiment5/number_conds/number_conds_vivid_victims.csv", 
                experimentID = "exp5a", 
                experimentDesc = "Experiment 5a", 
                Group = NA_character_, 
                rename_victims = FALSE
            ),
            list(
                relative_path = "experiment5/number_conds/number_conds_vivid_victims_5b.csv",
                experimentID = "exp5b",
                experimentDesc = "Experiment 5b",
                Group = NA_character_,
                rename_victims = FALSE
            ),
            list(
                relative_path = "experiment6/number_conds/number_conds_vivid_victims_exp6.csv",
                experimentID = "exp6",
                experimentDesc = "Experiment 6",
                Group = NA_character_,
                rename_victims = FALSE
            ),
            list(
                relative_path = "experiment7/number_conds_exp7_counterbalancing_group1.csv",
                experimentID = "exp7",
                experimentDesc = "Experiment 7",
                Group = "Group 1",
                rename_victims = FALSE
            ),
            list(
                relative_path = "experiment7/number_conds_exp7_counterbalancing_group2.csv",
                experimentID = "exp7",
                experimentDesc = "Experiment 7",
                Group = "Group 2",
                rename_victims = FALSE
            ),
            list(
                relative_path = "experiment8_incommensurate_scenarios/number_conds_exp8_counterbalancing_group1.csv",
                experimentID = "exp8",
                experimentDesc = "Experiment 8",
                Group = "Group 1",
                rename_victims = FALSE
            ),
            list(
                relative_path = "experiment8_incommensurate_scenarios/number_conds_exp8_counterbalancing_group2.csv",
                experimentID = "exp8",
                experimentDesc = "Experiment 8",
                Group = "Group 2",
                rename_victims = FALSE
            ),
            list(
                relative_path = "experiment9_verify_asymptote/number_conds_exp9a_counterbalancing_group1.csv",
                experimentID = "exp9a",
                experimentDesc = "Experiment 9a",
                Group = "Group 1",
                rename_victims = FALSE
            ),
            list(
                relative_path = "experiment9_verify_asymptote/number_conds_exp9a_counterbalancing_group2.csv",
                experimentID = "exp9a",
                experimentDesc = "Experiment 9a",
                Group = "Group 2",
                rename_victims = FALSE
            ),
            list(
                relative_path = "experiment10_incommensurate_scenarios_new_ratios/number_conds_exp10_counterbalancing_group1.csv",
                experimentID = "exp10",
                experimentDesc = "Experiment 10",
                Group = "Group 1",
                rename_victims = FALSE
            ),
            list(
                relative_path = "experiment10_incommensurate_scenarios_new_ratios/number_conds_exp10_counterbalancing_group2.csv",
                experimentID = "exp10",
                experimentDesc = "Experiment 10",
                Group = "Group 2",
                rename_victims = FALSE
            ),
            list(
                relative_path = "experiment11_incommensurate_scenarios_new_ratios_replication_of_exp10/number_conds_exp10_counterbalancing_group2.csv",
                experimentID = "exp11",
                experimentDesc = "Experiment 11",
                Group = "Group 1",
                rename_victims = FALSE
            ),
            list(
                relative_path = "experiment11_incommensurate_scenarios_new_ratios_replication_of_exp10/number_conds_exp10_counterbalancing_group2.csv",
                experimentID = "exp11",
                experimentDesc = "Experiment 11",
                Group = "Group 2",
                rename_victims = FALSE
            )
        ) %>% 
        tibble::enframe(name = NULL, value = "data") %>% 
        tidyr::unnest_wider(data) %>%
        # LOOP through the files using pmap_dfr
        purrr::pmap_dfr(\(relative_path, experimentID, experimentDesc, Group, rename_victims) {
            file_path <- here::here(
                EXPERIMENT_DIR,
                relative_path
            )
            
            df <- readr::read_csv(file_path, show_col_types = FALSE) %>%
                dplyr::mutate(
                    experimentID = experimentID,
                    experimentDesc = experimentDesc,
                    .before = 1
                )
            
            if (rlang::has_name(df, "ratio_saved_victims")) {
                df <- dplyr::mutate(df, ratio_saved_victims = as.character(ratio_saved_victims))
            }
            
            if (!is.na(Group)) {
                df <- dplyr::mutate(df, Group = Group, .before = 1)
            }
            
            if (rename_victims) {
                df <- dplyr::rename_with(df, ~ stringr::str_remove(.x, "1$"), dplyr::ends_with("1"))
            }
            
            return(df)
        }) %>%
        dplyr::select(experimentID, experimentDesc, Group, dplyr::starts_with("n_"), dplyr::starts_with("option")) %>%
        dplyr::mutate(
            ratio = n_saved / n_victims,
            # get.number.ratio(n_saved / n_victims, FALSE),
            .after = "n_total"
        ) %>%
        dplyr::mutate(
            ratio2 = n_saved2 / n_victims2,
            # get.number.ratio(n_saved2 / n_victims2, FALSE),
            .after = "n_total2"
        )
}


## ----load-scenarios------------------------------------------------------------------------------
if (RELOAD_DATA) {
    dat.scenario.files <- list(
        list(
            experimentID = "exp1",
            condition = NA_character_,
            file = here::here(
                EXPERIMENT_DIR,
                "experiment1", 
                "scenarios.xml"
            )
        ),
        list(
            experimentID = "exp1b",
            condition = NA_character_,
            file = here::here(
                EXPERIMENT_DIR,
                "experiment1b", "scenarios_replication.xml"
            )
        ),
        list(
            experimentID = "exp2",
            condition = "moral",
            file = here::here(
                EXPERIMENT_DIR,
                "experiment2_3", "final", "scenarios_xml", "Scenarios_Moral_Choice_Version.xml"
            )
        ),
        list(
            experimentID = "exp2",
            condition = "economic",
            file = here::here(
                EXPERIMENT_DIR,
                "experiment2_3", "final", "scenarios_xml", "Scenarios_Economic_Choice_Version.xml"
            )
        ),
        list(
            experimentID = "exp4",
            condition = "economic",
            file = here::here(
                EXPERIMENT_DIR,
                "experiment4", "scenarios_replication_economic.xml"
            )
        ),
        list(
            experimentID = "exp4",
            condition = "moral",
            file = here::here(
                EXPERIMENT_DIR,
                "experiment1b", "scenarios_replication.xml"
            )
        ),
        list(
            experimentID = "exp8",
            condition = NA_character_,
            file = here::here(
                EXPERIMENT_DIR,
                "experiment8_incommensurate_scenarios", "incommensurate_scenarios_single_horrificness_question.xml"
            )
        ),
        list(
            experimentID = "exp9a",
            condition = NA_character_,
            file = here::here(
                EXPERIMENT_DIR,
                "experiment9_verify_asymptote", "incommensurate_scenarios_single_horrificness_question.10.scenarios.xml"
            )
        ),
        list(
            experimentID = "exp10",
            condition = NA_character_,
            file = here::here(
                EXPERIMENT_DIR,
                "experiment10_incommensurate_scenarios_new_ratios", "incommensurate_scenarios_single_horrificness_question.new_questions.scenarios.exp10.xml"
            )
        ),
        list(
            experimentID = "exp11",
            condition = NA_character_,
            file = here::here(
                EXPERIMENT_DIR,
                "experiment11_incommensurate_scenarios_new_ratios_replication_of_exp10", "incommensurate_scenarios_single_horrificness_question.new_questions.scenarios.exp10.replication.xml"
            )
        )
    ) %>% 
        tibble::enframe(name = NULL, value = "data") %>% 
        tidyr::unnest_wider(data)
    
    
    
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
        dplyr::group_modify(~ .x$file %>%
                                xml2::read_xml(.) %>%
                                XML::xmlParse() %>%
                                XML::xmlToDataFrame()) %>%
        dplyr::ungroup() %>%
        tidyr::separate_wider_delim(label, names = c("label", "condition2"), delim = ".", too_few = "align_start") %>%
        dplyr::mutate(condition = dplyr::coalesce(condition, condition2)) %>%
        dplyr::select(experimentID, condition, title, text, dplyr::starts_with("question"), dplyr::starts_with("option")) %>%
        dplyr::select(-dplyr::matches("anchor")) %>%
        dplyr::mutate(question.acceptability = dplyr::coalesce(question, question.acceptability)) %>%
        dplyr::select(-question) %>%
        dplyr::rename(question.severity = question.severity.text) %>%
        tidyr::drop_na(title, text) %>%
        dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ .x %>%
                                        # stringr::str_remove_all("\\n") %>%
                                        stringr::str_remove_all("[\\t\\n]") %>%
                                        stringr::str_remove_all("\\[br\\]") %>%
                                        stringr::str_squish()))
}


## ----load-data-exp1ab----------------------------------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp1 <-
        dplyr::bind_rows(
            xlsx::read.xlsx(
                here::here(
                    DATA_DIR,
                    "moral_numbers.xlsx"
                ),
                sheetName = "Sheet0",
                header = TRUE,
                stringsAsFactors = FALSE
            ) %>%
                dplyr::filter(Finished == 1) %>%
                # Consent: Q149_1...8 == Yes
                dplyr::select(c(
                    ResponseId,
                    RecordedDate,
                    StartDate,
                    EndDate,
                    gender,
                    native.languages,
                    dplyr::starts_with("scenario")
                )) %>%
                dplyr::mutate(experimentID = "exp1") %>%
                dplyr::mutate(runID = "exp1"),
            xlsx::read.xlsx(
                here::here(
                    DATA_DIR,
                    "moral_numbers_replication.xlsx"
                ),
                sheetName = "Sheet0",
                header = TRUE,
                stringsAsFactors = FALSE
            ) %>%
                dplyr::filter(Finished == 1) %>%
                dplyr::rename(c(
                    "age" = "Q105",
                    "gender" = "Q106",
                    "native.languages" = "Q107"
                )) %>%
                # Consent: Q149_1...8 == Yes
                dplyr::select(c(
                    ResponseId,
                    RecordedDate,
                    StartDate,
                    EndDate,
                    gender,
                    native.languages,
                    dplyr::starts_with("scenario")
                )) %>%
                dplyr::mutate(experimentID = "exp1b") %>%
                dplyr::mutate(runID = "exp1b")
        ) %>%
        dplyr::rename(startDate = StartDate) %>%
        dplyr::rename(endDate = EndDate) %>%
        convert.dates.and.calculate.duration()
}
# dat.moral.numbers.exp1  %>% glimpse


## ----reformat-data-to-long-format-exp1ab---------------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp1 <- dat.moral.numbers.exp1 %>%
        # Convert ratings from strings to numbers
        dplyr::mutate_at(
            dplyr::vars(dplyr::starts_with("scenario")),
            function(X) gsub("^(\\d+).*", "\\1", X)
        ) %>%
        dplyr::mutate_at(
            dplyr::vars(dplyr::starts_with("scenario")),
            as.numeric
        ) %>%
        # Transform to long format
        tidyr::gather(condition.name, rating,
                      dplyr::starts_with("scenario"),
                      na.rm = TRUE
        ) %>%
        # Split scenario label into variables
        dplyr::mutate(scenarioID = gsub(
            "^scenario\\.(.*?)\\..*$",
            "\\1",
            condition.name,
            perl = TRUE
        )) %>%
        dplyr::mutate(n.saved = gsub(
            "^.*\\.n\\_saved\\.(\\d+)\\..*$",
            "\\1",
            condition.name,
            perl = TRUE
        )) %>%
        dplyr::mutate(n.victims = gsub(
            "^.*\\.n\\_victims\\.(\\d+)$",
            "\\1",
            condition.name,
            perl = TRUE
        )) %>%
        dplyr::mutate(n.total = gsub(
            "^.*\\.n\\_total\\.(\\d+)\\..*$",
            "\\1",
            condition.name,
            perl = TRUE
        )) %>%
        dplyr::mutate_at(
            dplyr::vars(dplyr::starts_with("n.")),
            as.numeric
        ) %>%
        dplyr::mutate(scenarioID = factor(scenarioID)) %>%
        dplyr::rename("rating.raw" = "rating") %>%
        dplyr::mutate(rating.bin = 1 * (rating.raw > 3)) %>%
        tibble::add_column(n.net.saved = NA, .after = "n.total") %>%
        tibble::add_column(ratio = NA, .before = "rating.raw") %>%
        tibble::add_column(p.saved = NA, .after = "ratio") %>%
        dplyr::mutate(n.net.saved = n.saved - n.victims) %>%
        dplyr::mutate(ratio = n.saved / n.victims) %>%
        dplyr::mutate(p.saved = n.saved / n.total)
}


## ----load-data-exp23-----------------------------------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp23 <-
        # Experiment 2a/3
        dplyr::bind_rows(
            read.csv2(
                here::here(
                    DATA_DIR,
                    "Moral+Numbers+(2a-Moral_Economic+choices-ratios+vs+utility+++evil+choice).csv"
                ),
                header = TRUE,
                sep = ",",
                stringsAsFactors = FALSE
            ) %>%
                # Relabel the questions
                dplyr::rename(all_of(l_moral_numbers_question_labels$exp2a3)) %>%
                dplyr::mutate(runID = "exp2a_3"),
            
            # Experiment 2b/3
            read.csv2(
                here::here(
                    DATA_DIR,
                    "Moral+Numbers+(2b-Moral_Economic+choices-ratios+vs.+number+of+victims+++evil+choice).csv"
                ),
                header = TRUE,
                sep = ",",
                stringsAsFactors = FALSE
            ) %>%
                # Relabel the questions
                dplyr::rename(all_of(l_moral_numbers_question_labels$exp2b3)) %>%
                dplyr::mutate(runID = "exp2b_3"),
            
            # Experiment 2c/3
            # This is a replication of Experiment 2a
            read.csv2(
                here::here(
                    DATA_DIR,
                    "Moral+Numbers+(2c-Moral_Economic+choices-ratios+vs+utility+++evil+choice+-+Replication).csv"
                ),
                header = TRUE,
                sep = ",",
                stringsAsFactors = FALSE
            ) %>%
                # Relabel the questions
                dplyr::rename(all_of(l_moral_numbers_question_labels$exp2c3)) %>%
                dplyr::mutate(runID = "exp2c_3"),
            
            # Experiment 2d/3b
            # Experiment 2d: Replication of Experiment 2a with less extreme ratios
            # Experiment 3b: Evil choices pitting ratios against utility
            read.csv2(
                here::here(
                    DATA_DIR,
                    "Moral+Numbers+(2d_3b-Moral_Economic+choices-ratios+vs+utility+replication+with+less+extreme+contrast).csv"
                ),
                header = TRUE,
                sep = ",",
                stringsAsFactors = FALSE
            ) %>%
                # Relabel the questions
                dplyr::rename(all_of(l_moral_numbers_question_labels$exp2d3b)) %>%
                dplyr::mutate(runID = "exp2d_3b")
        ) %>%
        dplyr::filter(finished == 1) %>%
        dplyr::rename(c(
            "age" = "QID5_TEXT",
            "gender" = "QID6",
            "native.languages" = "QID7_TEXT",
            "ResponseId" = "recordId",
            "RecordedDate" = "recordedDate",
            myopic.n.saved = "QID733_1",
            myopic.n.victims = "QID733_2"
        )) %>%
        dplyr::select(c(
            runID,
            ResponseId,
            RecordedDate,
            startDate,
            endDate,
            gender,
            age,
            native.languages,
            first.scenario.type,
            first.choice.option,
            number.condition.id,
            dplyr::starts_with("myopic"),
            dplyr::starts_with("scenario")
        )) %>%
        # dplyr::mutate(RecordedDate = as.POSIXct(RecordedDate, tz = "GMT", format = "%m/%d/%Y %H:%M"))
        convert.dates.and.calculate.duration()
}


## ----load-data-exp34-----------------------------------------------------------------------------
if (RELOAD_DATA) {
    # Experiment 4/3d
    # Experiment 4: Replication of Experiment 1b with moral and economic choices
    # Experiment 3d: Replication of 3a with in-group favoritism
    dat.moral.numbers.exp34 <- read.csv2(
        here::here(
            DATA_DIR,
            "Moral+Numbers+(4+-+Exp+1b+with+moral+vs.+economic+choices+++Exp+3d+Evil_Ingroup+favoritism).csv"
        ),
        header = TRUE,
        sep = ",",
        stringsAsFactors = FALSE
    ) %>%
        # Relabel the questions
        dplyr::rename(all_of(l_moral_numbers_question_labels$exp43d)) %>%
        dplyr::mutate(runID = "exp4_3c") %>%
        dplyr::filter(finished == 1) %>%
        dplyr::rename(c(
            "age" = "QID105_TEXT",
            "gender" = "QID106",
            "native.languages" = "QID107_TEXT",
            "ResponseId" = "recordId",
            "RecordedDate" = "recordedDate",
        )) %>%
        dplyr::select(c(
            runID,
            ResponseId,
            RecordedDate,
            startDate,
            endDate,
            gender,
            age,
            native.languages,
            first.scenario.type,
            current.scenario.type,
            dplyr::starts_with("scenario")
        )) %>%
        # dplyr::mutate(RecordedDate = as.POSIXct(RecordedDate, tz = "GMT", format = "%m/%d/%Y %H:%M"))
        convert.dates.and.calculate.duration()
}


## ----load-data-exp5-3d---------------------------------------------------------------------------
if (RELOAD_DATA) {
    # Experiment 5/3d
    # Experiment 5: Vivid vs. neutral scenarios
    # Experiment 3d: Replication of 3b with in-group favoritism
    dat.moral.numbers.exp35 <- read.csv2(
        here::here(            
            DATA_DIR,
            "Moral+Numbers+(5+-+vivid+and+neutral+moral+choices+++Exp+3d+Evil_Ingroup+favoritism).csv"
        ),
        header = TRUE,
        sep = ",",
        stringsAsFactors = FALSE
    ) %>%
        # Relabel the questions
        dplyr::rename(all_of(l_moral_numbers_question_labels$exp53d)) %>%
        dplyr::mutate(runID = "exp5_3d") %>%
        dplyr::filter(finished == 1) %>%
        dplyr::rename(c(
            "age" = "QID105_TEXT",
            "gender" = "QID106",
            "native.languages" = "QID107_TEXT",
            "ResponseId" = "recordId",
            "RecordedDate" = "recordedDate",
        )) %>%
        dplyr::select(c(
            runID,
            ResponseId,
            RecordedDate,
            startDate,
            endDate,
            gender,
            age,
            native.languages,
            first.scenario.type,
            current.scenario.type,
            dplyr::starts_with("scenario")
        )) %>%
        # dplyr::mutate(RecordedDate = as.POSIXct(RecordedDate, tz = "GMT", format = "%m/%d/%Y %H:%M"))
        convert.dates.and.calculate.duration()
}


## ----load-data-exp5b-----------------------------------------------------------------------------
if (RELOAD_DATA) {
    # Experiment 5b
    # Experiment 5: Vivid vs. neutral scenarios, with more ratios in the asymptotic phase and bold face for the sentences making the victims more vivid
    dat.moral.numbers.exp5b <- read.csv2(
        here::here(
            DATA_DIR,
            "Moral+Numbers+(5b+-+vivid+and+neutral+moral+choices+-+1+block+only+-+different+ratios).csv"
        ),
        header = TRUE,
        sep = ",",
        stringsAsFactors = FALSE
    ) %>%
        # Relabel the questions
        dplyr::rename(all_of(l_moral_numbers_question_labels$exp5b)) %>%
        dplyr::mutate(runID = "exp5b") %>%
        dplyr::filter(finished == 1) %>%
        dplyr::rename(c(
            "age" = "QID105_TEXT",
            "gender" = "QID106",
            "native.languages" = "QID107_TEXT",
            "ResponseId" = "recordId",
            "RecordedDate" = "recordedDate",
        )) %>%
        dplyr::select(c(
            runID,
            ResponseId,
            RecordedDate,
            startDate,
            endDate,
            gender,
            age,
            native.languages,
            first.scenario.type,
            current.scenario.type,
            dplyr::starts_with("scenario")
        )) %>%
        # dplyr::mutate(RecordedDate = as.POSIXct(RecordedDate, tz = "GMT", format = "%m/%d/%Y %H:%M"))
        convert.dates.and.calculate.duration()
}


## ----load-data-exp6------------------------------------------------------------------------------
if (RELOAD_DATA) {
    # Experiment 6
    # Experiment 6: Vivid vs. neutral scenarios, with more ratios in the asymptotic phase and bold face for the sentences making the victims more vivid. Vivid and neutral scenarios are presented in the first vs. third person voice
    dat.moral.numbers.exp6 <- read.csv2(
        here::here(            
            DATA_DIR,
            "Moral+Numbers+(6+-+vivid+and+neutral+moral+choices).csv"
        ),
        header = TRUE,
        sep = ",",
        stringsAsFactors = FALSE
    ) %>%
        # Relabel the questions
        dplyr::rename(all_of(l_moral_numbers_question_labels$exp6)) %>%
        dplyr::mutate(runID = "exp6") %>%
        dplyr::filter(finished == 1) %>%
        dplyr::rename(c(
            "age" = "QID105_TEXT",
            "gender" = "QID106",
            "native.languages" = "QID107_TEXT",
            "ResponseId" = "recordId",
            "RecordedDate" = "recordedDate",
        )) %>%
        dplyr::select(c(
            runID,
            ResponseId,
            RecordedDate,
            startDate,
            endDate,
            gender,
            age,
            native.languages,
            first.scenario.type,
            current.scenario.type,
            dplyr::starts_with("scenario")
        )) %>%
        # dplyr::mutate(RecordedDate = as.POSIXct(RecordedDate, tz = "GMT", format = "%m/%d/%Y %H:%M"))
        convert.dates.and.calculate.duration()
}


## ----load-data-exp7------------------------------------------------------------------------------
if (RELOAD_DATA) {
    # Experiment 7
    # Experiment 7: Vivid vs. neutral scenarios, with more ratios in the asymptotic phase and incommensurate deaths
    
    # We first load the file
    dat.moral.numbers.exp7 <- read.csv2(
        here::here(    
            DATA_DIR,
            "Moral+Numbers+(Exp+7+-+vivid+vs.+neutral+with+incomensurate+scenarios)_November+6,+2023_05.05.csv"
        ),
        header = TRUE,
        sep = ",",
        stringsAsFactors = FALSE
    ) %>%
        dplyr::mutate(runID = "exp7") %>%
        dplyr::filter(finished == 1) %>%
        dplyr::rename(c(
            "age" = "QID105_TEXT",
            "gender" = "QID106",
            "native.languages" = "QID107_TEXT",
            "ResponseId" = "X_recordId",
            "RecordedDate" = "recordedDate"
        ))
    
    # Before we can relabel the questions, we need to separate out the different sub-questions from the scenarios.
    
    l_moral_numbers_question_labels$exp7.with.subQuestions <- dplyr::left_join(
        l_moral_numbers_question_labels$exp7.no.subQuestions %>%
            tibble::enframe("label", "qid"),
        dat.moral.numbers.exp7 %>%
            names() %>%
            data.frame(col.name.orig = .) %>%
            dplyr::filter(stringr::str_starts(col.name.orig, "QID")) %>%
            tidyr::separate_wider_delim(col.name.orig,
                                        delim = "_", names = c("qid", "sub_id"),
                                        cols_remove = FALSE, too_few = "align_start", too_many = "drop"
            ),
        by = "qid"
    ) %>%
        dplyr::mutate(sub_id = dplyr::case_when(
            sub_id == "1" ~ "severity_saved",
            sub_id == "2" ~ "severity_victims",
            sub_id == "3" ~ "acceptability",
            TRUE ~ NA_character_
        )) %>%
        tidyr::unite(label, label, sub_id, sep = ".question.") %>%
        dplyr::select(label, col.name.orig) %>%
        tibble::deframe()
    
    # Now we are ready to relabel the questions
    
    dat.moral.numbers.exp7 <- dat.moral.numbers.exp7 %>%
        dplyr::rename(all_of(l_moral_numbers_question_labels$exp7.with.subQuestions)) %>%
        dplyr::mutate(
            passed.attention.check =
                (attentionCheckActual.question.severity_saved == 2) &
                (attentionCheckActual.question.severity_victims == 2) &
                (attentionCheckActual.question.acceptability == 2)
        ) %>%
        dplyr::select(c(
            runID,
            ResponseId,
            passed.attention.check,
            RecordedDate,
            startDate,
            endDate,
            gender,
            age,
            native.languages,
            first.scenario.type,
            number.counterbalancing.cond,
            current.scenario.type,
            dplyr::starts_with("scenario")
        )) %>%
        # dplyr::mutate(RecordedDate = as.POSIXct(RecordedDate, tz = "GMT", format = "%m/%d/%Y %H:%M"))
        convert.dates.and.calculate.duration()
}


## ----load-data-exp8------------------------------------------------------------------------------
if (RELOAD_DATA) {
    # Experiment 8
    # Experiment 8: Vivid vs. neutral scenarios, with more ratios in the asymptotic phase and incommensurate deaths and a single severity scale
    
    # We first load the file
    # We have two version of the file, one where the attention check was recorded in weird ways by qualtrics,
    # and one where it was recoded correctly. We combine them later on. The qsf file works for both versions.
    
    # In this version of the qualtrics file, the attention check was recoded in weird ways
    dat.moral.numbers.exp8.incorrect.attention.check <- read.csv2(
        here::here(    
            DATA_DIR,
            "Moral+Numbers+(Exp+8+-+vivid+vs.+neutral+with+incomensurate+scenarios)+-+with+wrong+recoding+of+atte_January+15,+2024_16.59.csv"
        ),
        header = TRUE,
        sep = ",",
        stringsAsFactors = FALSE
    ) %>%
        dplyr::mutate(runID = "exp8") %>%
        dplyr::filter(finished == 1) %>%
        dplyr::rename(c(
            "age" = "QID105_TEXT",
            "gender" = "QID106",
            "native.languages" = "QID107_TEXT",
            "ResponseId" = "X_recordId",
            "RecordedDate" = "recordedDate"
        )) %>%
        dplyr::rename(any_of(l_moral_numbers_question_labels$exp8)) %>%
        # We need to recode the attention check here since qualtrics recoded it in weird ways.
        dplyr::mutate(attChk.severity = dplyr::case_when(
            attChk.severity == 1 ~ 1,
            attChk.severity == 6 ~ 2,
            attChk.severity == 2 ~ 3,
            attChk.severity == 3 ~ 4,
            attChk.severity == 4 ~ 5,
            attChk.severity == 5 ~ 6,
            TRUE ~ NA_integer_
        )) %>%
        dplyr::mutate(attChk.acceptability = dplyr::case_when(
            attChk.acceptability == 1 ~ 1,
            attChk.acceptability == 6 ~ 2,
            attChk.acceptability == 2 ~ 3,
            attChk.acceptability == 3 ~ 4,
            attChk.acceptability == 4 ~ 5,
            attChk.acceptability == 5 ~ 6,
            TRUE ~ NA_integer_
        ))
    
    
    # In this version of the qualtrics file, the attention check was recoded correctly
    dat.moral.numbers.exp8.correct.attention.check <- read.csv2(
        here::here(
            DATA_DIR,
            "Moral+Numbers+(Exp+8+-+vivid+vs.+neutral+with+incomensurate+scenarios)+-+attention+check+corrected_January+15,+2024_16.58.csv"
        ),
        header = TRUE,
        sep = ",",
        stringsAsFactors = FALSE
    ) %>%
        dplyr::mutate(runID = "exp8") %>%
        dplyr::filter(finished == 1) %>%
        dplyr::rename(c(
            "age" = "QID105_TEXT",
            "gender" = "QID106",
            "native.languages" = "QID107_TEXT",
            "ResponseId" = "X_recordId",
            "RecordedDate" = "recordedDate"
        )) %>%
        dplyr::rename(any_of(l_moral_numbers_question_labels$exp8))
    
    # Combine the two versions
    
    dat.moral.numbers.exp8 <-
        dplyr::bind_rows(
            dat.moral.numbers.exp8.incorrect.attention.check,
            dat.moral.numbers.exp8.correct.attention.check
        ) %>%
        dplyr::mutate(
            passed.attention.check =
                (attChk.severity == 2) &
                (attChk.acceptability == 2)
        ) %>%
        dplyr::select(c(
            runID,
            ResponseId,
            passed.attention.check,
            RecordedDate,
            startDate,
            endDate,
            gender,
            age,
            native.languages,
            first.scenario.type,
            number.counterbalancing.cond,
            current.scenario.type,
            dplyr::starts_with("scenario")
        )) %>%
        # dplyr::mutate(RecordedDate = as.POSIXct(RecordedDate, tz = "GMT", format = "%m/%d/%Y %H:%M"))
        dplyr::mutate(gender = as.numeric(gender)) %>%
        dplyr::mutate(age = as.numeric(age)) %>%
        # dplyr::mutate(= = as.numeric()) %>%
        dplyr::mutate(number.counterbalancing.cond = as.numeric(number.counterbalancing.cond)) %>%
        # dplyr::mutate(dplyr::across(matches("Date"), as.Date))
        convert.dates.and.calculate.duration()
    
    # Remove the temporary versions to avoid problems down the road
    rm(dat.moral.numbers.exp8.correct.attention.check, dat.moral.numbers.exp8.incorrect.attention.check)
}


## ----load-data-exp9------------------------------------------------------------------------------
if (RELOAD_DATA) {
    # Experiment 9a
    # Experiment 9a: Vivid scenarios only, trying to identify asymptote
    
    
    dat.moral.numbers.exp9a <- read.csv2(
        here::here(            
            DATA_DIR,
            "Moral+Numbers+(Exp.+9a+-+verifying+asymptote+in+neutral+condition)_July+24,+2024_06.55.csv"
        ),
        header = TRUE,
        sep = ",",
        stringsAsFactors = FALSE
    ) %>%
        dplyr::mutate(runID = "exp9a") %>%
        dplyr::filter(finished == 1) %>%
        dplyr::rename(c(
            "age" = "QID105_TEXT",
            "gender" = "QID106",
            "native.languages" = "QID107_TEXT",
            "ResponseId" = "X_recordId",
            "RecordedDate" = "recordedDate"
        )) %>%
        dplyr::rename(any_of(l_moral_numbers_question_labels$exp9a)) %>%
        dplyr::mutate(
            passed.attention.check =
                (attChk.severity == 2) &
                (attChk.acceptability == 2)
        ) %>%
        dplyr::select(c(
            runID,
            ResponseId,
            passed.attention.check,
            RecordedDate,
            startDate,
            endDate,
            gender,
            age,
            native.languages,
            first.scenario.type,
            number.counterbalancing.cond,
            current.scenario.type,
            dplyr::starts_with("scenario")
        )) %>%
        # dplyr::mutate(RecordedDate = as.POSIXct(RecordedDate, tz = "GMT", format = "%m/%d/%Y %H:%M"))
        dplyr::mutate(gender = as.numeric(gender)) %>%
        dplyr::mutate(age = as.numeric(age)) %>%
        # dplyr::mutate(= = as.numeric()) %>%
        dplyr::mutate(number.counterbalancing.cond = as.numeric(number.counterbalancing.cond)) %>%
        # dplyr::mutate(dplyr::across(matches("Date"), as.Date))
        convert.dates.and.calculate.duration()
}


## ----load-data-exp10-----------------------------------------------------------------------------
if (RELOAD_DATA) {
    # Experiment 10
    # Experiment 10: Vivid vs. neutral scenarios, with more ratios in the asymptotic phase and incommensurate deaths and a single severity scale
    # Here we use higher ratios than in Exp 8
    
    # We first load the file
    # We have to versions of the experiment, one with wrong ratios for number counterbalancing condition 1, and one where it is corrected.
    
    dat.moral.numbers.exp10 <-
        dplyr::bind_rows(
            # Version where only number counterbalancing condition 2 has correct ratios
            # We will remove number counterbalancing condition 1 below.
            read.csv2(
                here::here(                    
                    DATA_DIR,
                    "Moral+Numbers+(Exp+10+-+vivid+vs.+neutral+with+incomensurate+scenarios,+higher+ratios))_September+6,+2024_13.17.csv"
                ),
                header = TRUE,
                sep = ",",
                stringsAsFactors = FALSE
            ) %>%
                dplyr::mutate(runID = "exp10.cb1.wrong") %>%
                dplyr::filter(finished == 1) %>%
                dplyr::rename(c(
                    "age" = "QID105_TEXT",
                    "gender" = "QID106",
                    "native.languages" = "QID107_TEXT",
                    "ResponseId" = "X_recordId",
                    "RecordedDate" = "recordedDate"
                )) %>%
                dplyr::rename(any_of(l_moral_numbers_question_labels$exp10)) %>%
                dplyr::mutate(
                    passed.attention.check =
                        (attChk.severity == 2) &
                        (attChk.acceptability == 2)
                ) %>%
                dplyr::select(c(
                    runID,
                    ResponseId,
                    passed.attention.check,
                    RecordedDate,
                    startDate,
                    endDate,
                    gender,
                    age,
                    native.languages,
                    first.scenario.type,
                    number.counterbalancing.cond,
                    current.scenario.type,
                    dplyr::starts_with("scenario")
                )) %>%
                # dplyr::mutate(RecordedDate = as.POSIXct(RecordedDate, tz = "GMT", format = "%m/%d/%Y %H:%M"))
                dplyr::mutate(gender = as.numeric(gender)) %>%
                dplyr::mutate(age = as.numeric(age)) %>%
                # dplyr::mutate(= = as.numeric()) %>%
                dplyr::mutate(number.counterbalancing.cond = as.numeric(number.counterbalancing.cond)) %>%
                # dplyr::mutate(dplyr::across(matches("Date"), as.Date))
                convert.dates.and.calculate.duration() %>%
                # The ratios for number.counterbalancing.cond == 1 ware wrong
                dplyr::filter(number.counterbalancing.cond == 2),
            
            # Version where both number counterbalancing conditions have correct ratios
            # These are only 12 participants; we will add more participants for counterbalancing condition 1 below
            read.csv2(
                here::here(                    
                    DATA_DIR,
                    "Moral+Numbers+(Exp+10+-+vivid+vs.+neutral+with+incomensurate+scenarios,+higher+ratios-+corrected)_September+8,+2024_18.09.csv"
                ),
                header = TRUE,
                sep = ",",
                stringsAsFactors = FALSE
            ) %>%
                dplyr::mutate(runID = "exp10") %>%
                dplyr::filter(finished == 1) %>%
                dplyr::rename(c(
                    "age" = "QID105_TEXT",
                    "gender" = "QID106",
                    "native.languages" = "QID107_TEXT",
                    "ResponseId" = "X_recordId",
                    "RecordedDate" = "recordedDate"
                )) %>%
                dplyr::rename(any_of(l_moral_numbers_question_labels$exp10.corrected)) %>%
                dplyr::mutate(
                    passed.attention.check =
                        (attChk.severity == 2) &
                        (attChk.acceptability == 2)
                ) %>%
                dplyr::select(c(
                    runID,
                    ResponseId,
                    passed.attention.check,
                    RecordedDate,
                    startDate,
                    endDate,
                    gender,
                    age,
                    native.languages,
                    first.scenario.type,
                    number.counterbalancing.cond,
                    current.scenario.type,
                    dplyr::starts_with("scenario")
                )) %>%
                # dplyr::mutate(RecordedDate = as.POSIXct(RecordedDate, tz = "GMT", format = "%m/%d/%Y %H:%M"))
                dplyr::mutate(gender = as.numeric(gender)) %>%
                dplyr::mutate(age = as.numeric(age)) %>%
                # dplyr::mutate(= = as.numeric()) %>%
                dplyr::mutate(number.counterbalancing.cond = as.numeric(number.counterbalancing.cond)) %>%
                # dplyr::mutate(dplyr::across(matches("Date"), as.Date))
                convert.dates.and.calculate.duration(),
            
            # Corrected ratios, counterbalancing condition 1 only.
            read.csv2(
                here::here(
                    DATA_DIR,
                    "Moral+Numbers+(Exp+10+-+vivid+vs.+neutral+with+incomensurate+scenarios,+higher+ratios-+correct,+cb1_September+11,+2024_15.13.csv"
                ),
                header = TRUE,
                sep = ",",
                stringsAsFactors = FALSE
            ) %>%
                dplyr::mutate(runID = "exp10.cb1.only") %>%
                dplyr::filter(finished == 1) %>%
                dplyr::rename(c(
                    "age" = "QID105_TEXT",
                    "gender" = "QID106",
                    "native.languages" = "QID107_TEXT",
                    "ResponseId" = "X_recordId",
                    "RecordedDate" = "recordedDate"
                )) %>%
                dplyr::rename(any_of(l_moral_numbers_question_labels$exp10.corrected.cb1.only)) %>%
                dplyr::mutate(
                    passed.attention.check =
                        (attChk.severity == 2) &
                        (attChk.acceptability == 2)
                ) %>%
                dplyr::select(c(
                    runID,
                    ResponseId,
                    passed.attention.check,
                    RecordedDate,
                    startDate,
                    endDate,
                    gender,
                    age,
                    native.languages,
                    first.scenario.type,
                    number.counterbalancing.cond,
                    current.scenario.type,
                    dplyr::starts_with("scenario")
                )) %>%
                # dplyr::mutate(RecordedDate = as.POSIXct(RecordedDate, tz = "GMT", format = "%m/%d/%Y %H:%M"))
                dplyr::mutate(gender = as.numeric(gender)) %>%
                dplyr::mutate(age = as.numeric(age)) %>%
                # dplyr::mutate(= = as.numeric()) %>%
                dplyr::mutate(number.counterbalancing.cond = as.numeric(number.counterbalancing.cond)) %>%
                # dplyr::mutate(dplyr::across(matches("Date"), as.Date))
                convert.dates.and.calculate.duration()
        )
}


## ----load-data-exp11-----------------------------------------------------------------------------
if (RELOAD_DATA) {
    # Replication of Experiment 10
    # Experiment 10: Vivid vs. neutral scenarios, with more ratios in the asymptotic phase and incommensurate deaths and a single severity scale
    # Here we use higher ratios than in Exp 8
    
    # We first load the file
    dat.moral.numbers.exp11 <-
        read.csv2(
            here::here(
                DATA_DIR,
                "exp11+(replication+of+exp10,+vivid+vs.+neutral)_December+12,+2025_10.56.csv"
            ),
            header = TRUE,
            sep = ",",
            stringsAsFactors = FALSE
        ) %>%
        dplyr::mutate(runID = "exp11") %>%
        dplyr::filter(finished == 1) %>%
        dplyr::rename(c(
            "age" = "QID105_TEXT",
            "gender" = "QID106",
            "native.languages" = "QID107_TEXT",
            "ResponseId" = "X_recordId",
            "RecordedDate" = "recordedDate",
            "strategies" = "QID3613_TEXT"
        )) %>%
        dplyr::rename(any_of(l_moral_numbers_question_labels$exp11)) %>%
        # dplyr::mutate(passed.attention.check =
        #                   (attChk.severity == 2) &
        #                   (attChk.acceptability == 2)) %>%
        # Values are recoded for some reason
        dplyr::mutate(
            passed.attention.check =
                (attChk.severity == 6) &
                (attChk.acceptability == 6)
        ) %>%
        dplyr::select(c(
            runID,
            ResponseId,
            passed.attention.check,
            RecordedDate,
            startDate,
            endDate,
            gender,
            age,
            native.languages,
            first.scenario.type,
            number.counterbalancing.cond,
            strategies,
            current.scenario.type,
            dplyr::starts_with("scenario")
        )) %>%
        # dplyr::mutate(RecordedDate = as.POSIXct(RecordedDate, tz = "GMT", format = "%m/%d/%Y %H:%M"))
        dplyr::mutate(gender = as.numeric(gender)) %>%
        dplyr::mutate(age = as.numeric(age)) %>%
        # dplyr::mutate(= = as.numeric()) %>%
        dplyr::mutate(number.counterbalancing.cond = as.numeric(number.counterbalancing.cond)) %>%
        # dplyr::mutate(dplyr::across(matches("Date"), as.Date))
        convert.dates.and.calculate.duration()
}


## ----reformat-data-to-long-format-exp23----------------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp23 <- dat.moral.numbers.exp23 %>%
        scenarios_to_long() %>%
        # avoid problems down the road with the dot in ratio_vs_utility_1.5utility_3xvictims
        dplyr::mutate(condition.name = stringr::str_replace_all(
            condition.name,
            fixed("ratio_vs_utility_1.5utility_3xvictims"),
            fixed("ratio_vs_utility")
        )) %>%
        dplyr::mutate(
            experimentID = dplyr::case_when(
                (runID == "exp2a_3") & stringr::str_detect(condition.name, "ratio_vs_utility") ~ "exp2a",
                (runID == "exp2c_3") & stringr::str_detect(condition.name, "ratio_vs_utility") ~ "exp2c",
                (runID == "exp2b_3") & stringr::str_detect(condition.name, "ratio_vs_victims") ~ "exp2b",
                (runID == "exp2d_3b") & stringr::str_detect(condition.name, "ratio_vs_utility") ~ "exp2d",
                (runID == "exp2d_3b") & stringr::str_detect(condition.name, "evil") ~ "exp3b",
                stringr::str_detect(condition.name, "evil") ~ "exp3"
            ),
            .before = 1
        )
}


## ----reformat-data-to-long-format-exp34----------------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp34 <- dat.moral.numbers.exp34 %>%
        scenarios_to_long() %>%
        dplyr::mutate(
            experimentID = dplyr::case_when(
                (runID == "exp4_3c") & stringr::str_detect(condition.name, "evil") ~ "exp3c",
                (runID == "exp4_3c") ~ "exp4"
            ),
            .before = 1
        )
}


## ----reformat-data-to-long-format-exp35----------------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp35 <- dat.moral.numbers.exp35 %>%
        scenarios_to_long() %>%
        dplyr::mutate(
            experimentID = dplyr::case_when(
                (runID == "exp5_3d") & stringr::str_detect(condition.name, "evil") ~ "exp3d",
                (runID == "exp5_3d") ~ "exp5"
            ),
            .before = 1
        )
}


## ----reformat-data-to-long-format-exp5b----------------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp5b <- dat.moral.numbers.exp5b %>%
        scenarios_to_long() %>%
        dplyr::mutate(
            experimentID = dplyr::case_when(
                (runID == "exp5b") ~ "exp5b"
            ),
            .before = 1
        )
}


## ----reformat-data-to-long-format-exp6-----------------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp6 <- dat.moral.numbers.exp6 %>%
        scenarios_to_long() %>%
        dplyr::mutate(
            experimentID = dplyr::case_when(
                (runID == "exp6") ~ "exp6"
            ),
            .before = 1
        )
}


## ----reformat-data-to-long-format-exp7-----------------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp7 <- dat.moral.numbers.exp7 %>%
        scenarios_to_long() %>%
        dplyr::mutate(
            experimentID = dplyr::case_when(
                (runID == "exp7") ~ "exp7"
            ),
            .before = 1
        )
}


## ----reformat-data-to-long-format-exp8-----------------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp8 <- dat.moral.numbers.exp8 %>%
        scenarios_to_long() %>%
        dplyr::mutate(
            experimentID = dplyr::case_when(
                (runID == "exp8") ~ "exp8"
            ),
            .before = 1
        )
}


## ----reformat-data-to-long-format-exp9-----------------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp9a <- dat.moral.numbers.exp9a %>%
        scenarios_to_long() %>%
        dplyr::mutate(
            experimentID = dplyr::case_when(
                (runID == "exp9a") ~ "exp9a"
            ),
            .before = 1
        )
}


## ----reformat-data-to-long-format-exp10----------------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp10 <- dat.moral.numbers.exp10 %>%
        scenarios_to_long() %>%
        dplyr::mutate(
            experimentID = dplyr::case_when(
                stringr::str_starts(runID, "exp10") ~ "exp10"
            ),
            .before = 1
        )
}


## ----reformat-data-to-long-format-exp11----------------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp11 <- dat.moral.numbers.exp11 %>%
        scenarios_to_long() %>%
        dplyr::mutate(
            experimentID = dplyr::case_when(
                stringr::str_starts(runID, "exp11") ~ "exp11"
            ),
            .before = 1
        )
}


## ----reformat-data-format-exp2-separate----------------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp2 <- dat.moral.numbers.exp23 %>%
        dplyr::filter(!stringr::str_detect(condition.name, "evil")) %>%
        split_condition_name(c("scenarioID", "decisionType", "comparisonType"), c(1, 4)) %>%
        dplyr::mutate(condition.name = stringr::str_replace_all(condition.name, "(\\d)\\.(\\d)", "\\1:\\2")) %>%
        dplyr::mutate(condition.name = stringr::str_replace_all(condition.name, "(high_ratio)", "ratioOrder:\\1")) %>%
        separate.multiple.field.value.pairs() %>%
        # Make label consistent for decisionType and first.scenario.type
        dplyr::mutate(decisionType = ifelse(stringr::str_detect(decisionType, "econ"), "economic", decisionType)) %>%
        # Add block
        dplyr::mutate(block = 2 - (decisionType == first.scenario.type)) %>%
        dplyr::mutate(block = factor(block)) %>%
        dplyr::rename_with(~ stringr::str_replace(.x, "n_", "n."), dplyr::starts_with("n_")) %>%
        dplyr::mutate(dplyr::across(dplyr::starts_with("n."), as.numeric)) %>%
        dplyr::mutate(scenarioID = factor(scenarioID)) %>%
        # Recode values; high values correspond to acceptance of high ratio option
        dplyr::mutate(
            rating.raw = ifelse(ratioOrder == "high_ratio_second",
                                rating,
                                7 - rating
            ),
            rating.bin = 1 * (rating.raw > 3)
        ) %>%
        dplyr::select(-rating) %>%
        dplyr::mutate(
            n.net.saved1 = n.saved1 - n.victims1,
            n.net.saved2 = n.saved2 - n.victims2,
            ratio2 = n.saved2 / n.victims2,
            ratio1 = n.saved1 / n.victims1,
            p.saved1 = n.saved1 / n.total1,
            p.saved2 = n.saved2 / n.total2,
            ratio.of.ratios = ratio1 / ratio2,
            ratio.of.net.saved = n.net.saved2 / n.net.saved1,
            d.ratios = ratio1 - ratio2,
            d.net.saved = n.net.saved2 - n.net.saved1,
            .after = "n.victims2"
        )
}


## ----reformat-data-format-exp3-separate----------------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp3 <- dplyr::bind_rows(
        dat.moral.numbers.exp23 %>%
            dplyr::filter(stringr::str_detect(condition.name, "evil")),
        dat.moral.numbers.exp34 %>%
            dplyr::filter(stringr::str_detect(condition.name, "evil")) %>%
            dplyr::filter(experimentID == "exp3c"),
        dat.moral.numbers.exp35 %>%
            dplyr::filter(stringr::str_detect(condition.name, "evil")) %>%
            dplyr::filter(experimentID == "exp3d")
    ) %>%
        split_condition_name(c("scenarioID", "decisionType")) %>%
        # Fix typo
        dplyr::mutate(condition.name = stringr::str_replace(condition.name, "n_total1", "n_total")) %>%
        dplyr::mutate(condition.name = stringr::str_replace_all(condition.name, "(conditionPair)\\.", "\\1:")) %>%
        dplyr::mutate(condition.name = stringr::str_replace_all(condition.name, "(conditionType)\\.", "\\1:")) %>%
        separate.multiple.field.value.pairs() %>%
        dplyr::mutate(scenarioID = factor(scenarioID)) %>%
        calculate_number_conds()
}


## ----reformat-data-to-long-format-exp4-separate--------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp4 <- dat.moral.numbers.exp34 %>%
        dplyr::filter(experimentID == "exp4") %>%
        dplyr::filter(!stringr::str_detect(condition.name, "evil")) %>%
        split_condition_name() %>%
        # Fix typo
        # mutate(condition.name = stringr::str_replace(condition.name, "n_total1", "n_total")) %>%
        dplyr::mutate(condition.name = stringr::str_replace_all(condition.name, "(conditionPair)\\.", "\\1:")) %>%
        dplyr::mutate(condition.name = stringr::str_replace_all(condition.name, "(conditionType)\\.", "\\1:")) %>%
        dplyr::mutate(
            decisionType = ifelse(stringr::str_starts(condition.name, "econ"),
                                  "economic",
                                  "moral"
            ),
            .after = "scenarioID"
        ) %>%
        dplyr::mutate(condition.name = stringr::str_remove(condition.name, "^econ\\.")) %>%
        separate.multiple.field.value.pairs() %>%
        dplyr::mutate(scenarioID = factor(scenarioID)) %>%
        calculate_number_conds()
}


## ----reformat-data-to-long-format-exp5-separate--------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp5 <- dat.moral.numbers.exp35 %>%
        dplyr::filter(experimentID == "exp5") %>%
        dplyr::filter(!stringr::str_detect(condition.name, "evil")) %>%
        split_condition_name() %>%
        define_vivacity_col() %>%
        separate.multiple.field.value.pairs() %>%
        dplyr::mutate(scenarioID = factor(scenarioID)) %>%
        calculate_number_conds()
}


## ----reformat-data-to-long-format-exp5b-separate-------------------------------------------------
if (RELOAD_DATA) {
    # The data is already separated but we include this chunk for consistency of what is done in which chunks
    
    dat.moral.numbers.exp5b <- dat.moral.numbers.exp5b %>%
        dplyr::filter(experimentID == "exp5b") %>%
        split_condition_name() %>%
        define_vivacity_col() %>%
        separate.multiple.field.value.pairs() %>%
        dplyr::mutate(scenarioID = factor(scenarioID)) %>%
        calculate_number_conds()
}


## ----reformat-data-to-long-format-exp6-separate--------------------------------------------------
if (RELOAD_DATA) {
    # The data is already separated but we include this chunk for consistency of what is done in which chunks
    
    dat.moral.numbers.exp6 <- dat.moral.numbers.exp6 %>%
        dplyr::filter(experimentID == "exp6") %>%
        split_condition_name() %>%
        define_vivacity_col() %>%
        separate.multiple.field.value.pairs() %>%
        dplyr::mutate(scenarioID = factor(scenarioID)) %>%
        calculate_number_conds()
}


## ----reformat-data-to-long-format-exp7-separate--------------------------------------------------
if (RELOAD_DATA) {
    # The data is already separated but we include this chunk for consistency of what is done in which chunks
    
    dat.moral.numbers.exp7 <- dat.moral.numbers.exp7 %>%
        dplyr::filter(experimentID == "exp7") %>%
        split_condition_name() %>%
        define_vivacity_col() %>%
        # remove ratio_saved_victims as it will be computed later and will trip the name value separation
        dplyr::mutate(condition.name = stringr::str_remove(condition.name, "\\.ratio_saved_victims\\.\\d+(\\.\\d+)*")) %>%
        dplyr::mutate(condition.name = stringr::str_replace(condition.name, "(\\.question)\\.", "\\1:")) %>%
        separate.multiple.field.value.pairs() %>%
        dplyr::mutate(scenarioID = factor(scenarioID)) %>%
        calculate_number_conds()
}


## ----reformat-data-to-long-format-exp8-separate--------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp8 <- dat.moral.numbers.exp8 %>%
        dplyr::filter(experimentID == "exp8") %>%
        split_condition_name() %>%
        define_vivacity_col() %>%
        # Now put the severity/acceptability IDs into separate columns
        define_question_col() %>%
        # remove ratio_saved_victims as it will be computed later and will trip the name value separation
        dplyr::mutate(condition.name = stringr::str_remove(condition.name, "\\.ratio_saved_victims\\.\\d+(\\.\\d+)*")) %>%
        separate.multiple.field.value.pairs() %>%
        dplyr::mutate(scenarioID = factor(scenarioID)) %>%
        calculate_number_conds()
}


## ----reformat-data-to-long-format-exp9-separate--------------------------------------------------
if (RELOAD_DATA) {
    # v.moral.numbers.exp9a.questionLabels %>% names %>% grepv("scenario", .) %>% stringr::str_remove_all("^scenario\\..*group\\.\\d\\.") %>% grepv("text", ., invert = TRUE) %>% stringr::str_remove_all("severity") %>% stringr::str_remove_all("acceptability") %>% unique()
    
    # in v.moral.numbers.exp9a.questionLabels, there are a few labels ending in .text
    # Check if they exist in dat.moral.numbers.exp9a
    # dat.moral.numbers.exp9a$condition.name %>% grepv(".text$", .)
    # Nope, they don't
    
    
    dat.moral.numbers.exp9a <- dat.moral.numbers.exp9a %>%
        dplyr::filter(experimentID == "exp9a") %>%
        split_condition_name() %>%
        define_vivacity_col() %>%
        # Now put the severity/acceptability IDs into separate columns
        define_question_col() %>%
        # remove ratio_saved_victims as it will be computed later and will trip the name value separation
        dplyr::mutate(condition.name = stringr::str_remove(condition.name, "\\.ratio_saved_victims\\.\\d+(\\.\\d+)*")) %>%
        separate.multiple.field.value.pairs() %>%
        dplyr::mutate(scenarioID = factor(scenarioID)) %>%
        calculate_number_conds()
}


## ----reformat-data-to-long-format-exp10-separate-------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp10 <- dat.moral.numbers.exp10 %>%
        dplyr::filter(experimentID == "exp10") %>%
        split_condition_name() %>%
        define_vivacity_col() %>%
        # Now put the severity/acceptability IDs into separate columns
        define_question_col() %>%
        # remove ratio_saved_victims as it will be computed later and will trip the name value separation
        # but keep theoretical ratio that is in the scenario for debugging purposes
        dplyr::mutate(ratio.theoretical = as.numeric(stringr::str_replace(condition.name, "^.*\\.ratio_saved_victims\\.(\\d+(\\.\\d+)*)", "\\1"))) %>%
        dplyr::mutate(condition.name = stringr::str_remove(condition.name, "\\.ratio_saved_victims\\.\\d+(\\.\\d+)*")) %>%
        separate.multiple.field.value.pairs() %>%
        dplyr::mutate(scenarioID = factor(scenarioID)) %>%
        calculate_number_conds()
    
    # dat.moral.numbers.exp10 %>%
    #     dplyr::count(question, vivacity, ratio)
    
    # dat.moral.numbers.exp10 %>%
    #     dplyr::filter(number.counterbalancing.cond == 1) %>%
    #     dplyr::select(n.saved, n.victims, ratio, ratio.theoretical) %>%
    #     # dplyr::group_by(ratio) %>%
    #     # dplyr::count()
    #     dplyr::filter(ratio != ratio.theoretical) %>%
    #     dplyr::distinct()
}


## ----reformat-data-to-long-format-exp11-separate-------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers.exp11 <- dat.moral.numbers.exp11 %>%
        dplyr::filter(experimentID == "exp11") %>%
        split_condition_name() %>%
        define_vivacity_col() %>%
        # Now put the severity/acceptability IDs into separate columns
        define_question_col() %>%
        # remove ratio_saved_victims as it will be computed later and will trip the name value separation
        # but keep theoretical ratio that is in the scenario for debugging purposes
        dplyr::mutate(ratio.theoretical = as.numeric(stringr::str_replace(condition.name, "^.*\\.ratio_saved_victims\\.(\\d+(\\.\\d+)*)", "\\1"))) %>%
        dplyr::mutate(condition.name = stringr::str_remove(condition.name, "\\.ratio_saved_victims\\.\\d+(\\.\\d+)*")) %>%
        separate.multiple.field.value.pairs() %>%
        dplyr::mutate(scenarioID = factor(scenarioID)) %>%
        calculate_number_conds()
    
    # dat.moral.numbers.exp11 %>%
    #     dplyr::count(question, vivacity, ratio)
    
    # dat.moral.numbers.exp11 %>%
    #     dplyr::select(number.counterbalancing.cond, n.saved, n.victims, ratio, ratio.theoretical) %>%
    #
    #     # dplyr::group_by(ratio) %>%
    #     # dplyr::count()
    #
    #     # dplyr::filter(ratio != ratio.theoretical) %>%
    #     # dplyr::mutate(d = abs(ratio  - ratio.theoretical))
    #
    #     dplyr::distinct() %>%
    #     dplyr::arrange(number.counterbalancing.cond, ratio, n.victims, n.saved)
}


## ----combine-experiments-------------------------------------------------------------------------
if (RELOAD_DATA) {
    dat.moral.numbers <- dplyr::bind_rows(
        dat.moral.numbers.exp1,
        dat.moral.numbers.exp2,
        dat.moral.numbers.exp3,
        dat.moral.numbers.exp4,
        dat.moral.numbers.exp5,
        dat.moral.numbers.exp5b,
        dat.moral.numbers.exp6,
        dat.moral.numbers.exp7,
        dat.moral.numbers.exp8,
        dat.moral.numbers.exp9a,
        dat.moral.numbers.exp10,
        dat.moral.numbers.exp11
    ) %>%
        # except in experiment 7, we ask only about acceptability
        dplyr::mutate(question = ifelse(is.na(question), "acceptability", question))
    
    
    # Remove these dfs so we process data consistently below
    rm(
        dat.moral.numbers.exp1,
        dat.moral.numbers.exp2,
        dat.moral.numbers.exp3,
        dat.moral.numbers.exp4,
        dat.moral.numbers.exp5,
        dat.moral.numbers.exp5b,
        dat.moral.numbers.exp6,
        dat.moral.numbers.exp7,
        dat.moral.numbers.exp8,
        dat.moral.numbers.exp9a,
        dat.moral.numbers.exp10,
        dat.moral.numbers.exp11
    )
}


## ----save-or-load-data---------------------------------------------------------------------------
if (RELOAD_DATA) {
    # Save data
    save(
        l_moral_numbers_question_labels,
        dat.number.conds,
        dat.scenario.files,
        dat.scenario.content,
        dat.moral.numbers,
        file = here::here(
            OUTPUT_DIR, 
            "moral_numbers_saved_data.RData"
        )
    )
} else {
    # Load it
    load(here::here(
        OUTPUT_DIR, 
        "moral_numbers_saved_data.RData"
    )
    )
}


## ----track-correspondance-between-experiments-in-paper-and-in-the-log-files----------------------

dat.moral.numbers.exp.correspondance <- list(
    # Basic experiment
    list(
        experimentID.data = "1", 
        experimentID.paper = "1a"
    ),
    # Replication with the conditions related to small number processing and the first item removed
    list(
        experimentID.data = "1b", 
        experimentID.paper = "1b"
    ),
    # 2a and 2b are switched
    # Ratio vs. victims
    list(
        experimentID.data = "2b", 
        experimentID.paper = "3a"
    ),
    # list(
    #     experimentID.data = "2a", 
    #     experimentID.paper = "2b"
    # ),
    # list(
    #     experimentID.data = "2c", 
    #     experimentID.paper = "2c"
    # ),
    # Ratio vs. utility
    # -   Exp 2a and 2c are replications of one another.
    # -   Exp 2d: Smaller differences in the number of victims
    # -   Exp 2b: Vs victims
    # -   Order in paper (2b => 2a), (2a,c,d = > 2bcd)
    list(
        experimentID.data = "2a", 
        experimentID.paper = "3b.1"
    ),
    list(
        experimentID.data = "2c", 
        experimentID.paper = "3b.2"
    ),
    # This is the combined experiment
    list(
        experimentID.data = "2ac", 
        experimentID.paper = "3b"
    ),
    list(
        experimentID.data = "2d", 
        experimentID.paper = "3c"
    ),
    # Moral vs. economic choices
    list(
        experimentID.data = "4", 
        experimentID.paper = "2"
    ),
    # Failed attempts to make the victims salient: 5 - 7
    # list(
    #     experimentID.data = "5", 
    #     experimentID.paper = "-1"
    # ),
    # list(
    #     experimentID.data = "6", 
    #     experimentID.paper = "-2"
    # ),
    # list(
    #     experimentID.data = "7", 
    #     experimentID.paper = "-3"
    # ),
    # Succesful attempt to make victims salient
    list(
        experimentID.data = "8", 
        experimentID.paper = "4"
    ),
    # Search for asymptote
    # list(
    #     experimentID.data = "9a", 
    #     experimentID.paper = "for SI"
    # ),
    list(
        experimentID.data = "9a", 
        experimentID.paper = "AS" # AS = asymptote search
    ),
    # Succesful attempt to make victims salient with higher ratios
    list(
        experimentID.data = "10", 
        experimentID.paper = "5"
    ),
    # Replication of experiment 10
    list(
        experimentID.data = "11",
        experimentID.paper = "6"
    ),
    # Combined vivacity experiments (8 + 10, 10 + 11)
    list(
        experimentID.data = "8+10",
        experimentID.paper = "4+5"
    ),
    list(
        experimentID.data = "10+11",
        experimentID.paper = "5+6"
    )
) %>%
    tibble::enframe(name = NULL, value = "data") %>%
    tidyr::unnest_wider(data) %>%
    dplyr::mutate(dplyr::across(dplyr::everything(), ~ stringr::str_c("exp", .x)))



## ----remove-incomplete-subjects------------------------------------------------------------------
dat.incomplete.subj <- dat.moral.numbers %>%
    dplyr::select(experimentID, ResponseId, rating.raw) %>%
    dplyr::group_by(experimentID, ResponseId) %>%
    dplyr::summarize(N = sum(is.finite(rating.raw))) %>%
    dplyr::filter(N != TRIALS_PER_EXP[experimentID])

if (REMOVE_INCOMPLETE_SUBJ) {
    dat.moral.numbers <- dplyr::anti_join(
        dat.moral.numbers,
        dat.incomplete.subj,
        by = c("experimentID", "ResponseId")
    )
}


## ----remove-subjects-with-no-variability---------------------------------------------------------
dat.invariable.subj <- dat.moral.numbers %>%
    dplyr::select(experimentID, ResponseId, rating.raw) %>%
    dplyr::group_by(experimentID, ResponseId) %>%
    dplyr::summarize(
        rating.raw = var(rating.raw),
        .groups = "drop"
    ) %>%
    dplyr::filter(rating.raw == 0)

if (REMOVE_CONSTANT_SUBJ) {
    dat.moral.numbers <- dplyr::anti_join(
        dat.moral.numbers,
        dat.invariable.subj,
        by = c("experimentID", "ResponseId")
    )
}


## ----remove-participants-with-unusual-completion-durations---------------------------------------
dat.moral.numbers.durations.median <- dat.moral.numbers %>%
    dplyr::group_by(experimentID) %>%
    dplyr::summarize(
        duration.median = median(duration),
        duration.mad = mad(duration),
        duration.mean = mean(duration),
        duration.sd = sd(duration),
        .groups = "drop"
    )

dat.fast.subj <- dplyr::left_join(
    dat.moral.numbers %>%
        dplyr::ungroup() %>%
        dplyr::select(experimentID, ResponseId, duration) %>%
        dplyr::distinct(),
    dat.moral.numbers.durations.median,
    by = "experimentID"
) %>%
    dplyr::mutate(dplyr::across(dplyr::where(~ is(.x, "difftime")), as.double, units = "mins")) %>%
    dplyr::filter(duration < (duration.median / 4))

if (REMOVE_FAST_SUBJ) {
    dat.moral.numbers <- dplyr::anti_join(
        dat.moral.numbers,
        dat.fast.subj,
        by = c("experimentID", "ResponseId")
    )
}


## ----calculate-demographics-initial--------------------------------------------------------------
dat.moral.numbers.demographics <- dat.moral.numbers %>%
    dplyr::ungroup() %>%
    dplyr::mutate(gender = dplyr::case_when(
        gender == 1 ~ "Male",
        gender == 2 ~ "Female",
        gender == 3 ~ "Other",
        gender == 4 ~ "Undisclosed",
        TRUE ~ NA_character_
    )) %>%
    get.demographics2(ResponseId, gender, age, experimentID)


## ----check-attention-check-----------------------------------------------------------------------
l_exp_with_attention_check <- dat.moral.numbers %>%
    dplyr::filter(!is.na(passed.attention.check)) %>%
    dplyr::distinct(experimentID) %>%
    dplyr::pull(experimentID)

dat.moral.numbers.failed.attention.check <- dat.moral.numbers %>%
    dplyr::filter(experimentID %in% l_exp_with_attention_check) %>%
    dplyr::filter(!passed.attention.check) %>%
    dplyr::distinct(experimentID, ResponseId)


if (nrow(dat.moral.numbers.failed.attention.check) > 0) {
    dat.moral.numbers <- dplyr::anti_join(
        dat.moral.numbers,
        dat.moral.numbers.failed.attention.check,
        by = c("experimentID", "ResponseId")
    )
}


## ----detect-ratios-------------------------------------------------------------------------------
dat.moral.numbers.ratios <- dat.moral.numbers %>%
    dplyr::distinct(experimentID, ratio) %>%
    dplyr::arrange(experimentID, ratio)

l.moral.numbers.ratios <- dat.moral.numbers.ratios %>%
    split(.$experimentID) %>%
    purrr::map(~ sort(.x$ratio))


## ----calculate-averages-except-exp2--------------------------------------------------------------
# Averages for Experiment 2 will be calculated below as the paradigm is different and involves choices between two options
# Note that we don't keep an extra grouping variable as each participant completes each ratio only once.

dat.moral.numbers.m.across.subj <- dplyr::bind_rows(
    # Collapse across blocks for experiments that have multiple blocks
    dat.moral.numbers %>%
        dplyr::filter(!stringr::str_starts(experimentID, "exp2")) %>%
        # Remove severity questions for experiments that have them
        dplyr::filter(question == "acceptability") %>%
        # Note that decisionType is specific to Experiment 4, while vivacity is specific to Experiment 5 and 6
        # dplyr::group_by(experimentID, decisionType, vivacity, ratio, n.victims, n.saved) %>%
        # added question (horrificness + acceptability)  for exp 7
        dplyr::group_by(experimentID, decisionType, vivacity, ratio, n.victims, n.saved, question) %>%
        summarize.ratings(.groups = "drop") %>%
        # Below, we add data for first blocks only
        dplyr::mutate(blocks = "all", .after = "experimentID"),
    
    # Combine all versions of Experiment 1
    dat.moral.numbers %>%
        dplyr::filter(stringr::str_starts(experimentID, "exp1(b)*$")) %>%
        dplyr::mutate(experimentID = stringr::str_replace(experimentID, "^exp1.*", "exp1-combined")) %>%
        dplyr::group_by(experimentID, decisionType, vivacity, ratio, n.victims, n.saved, question) %>%
        summarize.ratings(.groups = "drop") %>%
        # Below, we add data for first blocks only
        dplyr::mutate(blocks = "all", .after = "experimentID"),
    
    # First block only: exp4 (decisionType condition)
    dat.moral.numbers %>%
        dplyr::filter(experimentID == "exp4") %>%
        dplyr::filter(first.scenario.type == decisionType) %>%
        dplyr::group_by(experimentID, decisionType, vivacity, ratio, n.victims, n.saved, question) %>%
        summarize.ratings(.groups = "drop") %>%
        dplyr::mutate(blocks = "first", .after = "experimentID"),
    
    # First block only: all experiments with a vivacity condition (exp5, exp5b, exp6, exp7, exp8, exp10, exp11)
    dat.moral.numbers %>%
        # Filter !is.na(vivacity) is not needed but  makes the intent readable, then select the first block
        dplyr::filter(!is.na(vivacity)) %>%
        dplyr::filter(first.scenario.type == vivacity) %>%
        dplyr::group_by(experimentID, decisionType, vivacity, ratio, n.victims, n.saved, question) %>%
        summarize.ratings(.groups = "drop") %>%
        dplyr::mutate(blocks = "first", .after = "experimentID"),
    
    # First block of Experiment 5 and 5b combined
    dat.moral.numbers %>%
        dplyr::filter(stringr::str_starts(experimentID, "exp5")) %>%
        dplyr::mutate(experimentID = stringr::str_replace(experimentID, "^exp5.*", "exp5ab")) %>%
        dplyr::filter(first.scenario.type == vivacity) %>%
        dplyr::group_by(experimentID, decisionType, vivacity, ratio, n.victims, n.saved, question) %>%
        summarize.ratings(.groups = "drop") %>%
        dplyr::mutate(blocks = "first", .after = "experimentID")
) %>%
    dplyr::filter(question == "acceptability") %>%
    dplyr::select(-question) %>%
    dplyr::arrange(experimentID, blocks)


## ----overall-averages-by-participant-calculate---------------------------------------------------
dat.moral.numbers.m.overall.by.participant <- dat.moral.numbers %>%
    dplyr::filter(question == "acceptability") %>%
    dplyr::group_by(experimentID, ResponseId) %>%
    dplyr::summarize(
        rating.raw = mean(rating.raw),
        rating.bin = mean(rating.bin),
        .groups = "drop"
    ) %>%
    dplyr::arrange(rating.raw) %>%
    as.data.frame()

dat.moral.numbers.m.overall.by.participant.long <-
    dat.moral.numbers.m.overall.by.participant %>%
    tidyr::gather(rating.type, rating, c("rating.raw", "rating.bin"))


## ----overall-averages-by-scenario-calculate------------------------------------------------------
dat.moral.numbers.m.overall.by.scenario <- dat.moral.numbers %>%
    # Remove severity questions for experiments that have them
    dplyr::filter(question == "acceptability") %>%
    dplyr::group_by(experimentID, scenarioID) %>%
    dplyr::summarize(
        rating.raw = mean(rating.raw),
        rating.bin = mean(rating.bin),
        .groups = "drop"
    ) %>%
    dplyr::arrange(rating.raw) %>%
    as.data.frame()

dat.moral.numbers.m.overall.by.scenario.long <-
    dat.moral.numbers.m.overall.by.scenario %>%
    tidyr::gather(rating.type, rating, c("rating.raw", "rating.bin"))


# remove this typo, but I just care about exp 7 now


## ----define-ws-for-demos-------------------------------------------------------------------------

# Define all values of w for the demos as they will be used both for the 
# basic model illustration and for the loss aversion demonstration

w_demo_levels <- c(
    seq(0, 2, .5),
    10, 20, 50, 100, Inf
) 



## ----utilitarian-deontological-plot-define-------------------------------------------------------
plot.utilitarian.deontological <- expand.grid(
    ratio = seq(1, 40, .1),
    w = c(0, .5, 1, 2, Inf)
) %>%
    dplyr::mutate(
        acceptability =
            acceptability.fnc(
                w = w,
                a = 1,
                ratio = ratio
            )
    ) %>%
    dplyr::mutate(w = factor(
        w,
        levels = w_demo_levels)) %>% 
    ggplot2::ggplot(ggplot2::aes(x = ratio, y = acceptability, group = w, col = w, lty = w)) +
    ggplot2::geom_line() + # (size = 1.5) +
    ggplot2::ylim(0, 1) +
    #    ggplot2::scale_color_discrete((name = TeX("$w$")) +
    #    theme_black (base_size = 16) +
    # theme_classic(16) +
    ggplot2::labs(
        x = "Weber ratio",
        y = "Acceptability"
    ) +
    ggplot2::scale_color_discrete("Internal noise (w)", drop = FALSE) +
    ggplot2::scale_linetype_discrete("Internal noise (w)", drop = FALSE) +
    ggplot2::theme( # legend.position = c(.925, .325),
        legend.position = c(.75, .325),
        # legend.direction = "horizontal",
        legend.background = ggplot2::element_blank()
    ) +
    # legend.direction = "horizontal") +
    ggplot2::theme(legend.key.width = ggplot2::unit(.5, "cm")) +
    ggplot2::guides(colour = ggplot2::guide_legend(title.position = "top"))
# theme (title = ggplot2::element_blank()),


## ----loss-aversion-demo-define-functions, include = FALSE----------------------------------------
# Functions moved to helper_functions/moral_numbers_prospect.R


## ----loss-aversion-demo-create-data--------------------------------------------------------------
dat_prospect_demo <- create_data_for_prospect_demo(
    100, 
    ws = c(
        seq(0, 2, .5),
        10, 20, 50, 100, Inf
    ) 
) %>% 
    dplyr::mutate(
        w = factor(w, levels = w_demo_levels)
    )



## ----criterion-shift-plot-define-----------------------------------------------------------------
plot.criterion.shift <- expand.grid(
    ratio = seq(1, 40, .1),
    a = 2^seq(-2, 2, .5),
    w = c(.25, .5, .75)
) %>%
    dplyr::mutate(
        acceptability =
            acceptability.fnc(
                w = w,
                a = a,
                ratio = ratio
            )
    ) %>%
    dplyr::mutate(w = paste0("w = ", w)) %>%
    ggplot2::ggplot(ggplot2::aes(x = ratio, y = acceptability, group = a, col = a)) +
    ggplot2::geom_line() + # (size = 1.5) +
    ggplot2::ylim(0, 1) +
    ggplot2::labs(
        x = "Weber ratio",
        y = "Acceptability"
    ) +
    ggplot2::scale_color_gradient(
        name = TeX("Weight of victims ($\\alpha$)"),
        trans = "log2",
        low = "blue", high = "red"
    ) +
    #    theme_black (base_size = 16) +
    ggplot2::theme(
        legend.position = c(.925, .325),
        legend.background = ggplot2::element_blank()
    ) +
    # legend.direction = "horizontal") +
    ggplot2::facet_wrap(~w, ncol = 1) +
    ggplot2::theme(legend.key.width = ggplot2::unit(.5, "cm")) +
    ggplot2::guides(colour = ggplot2::guide_legend(title.position = "top"))
# theme (title = ggplot2::element_blank()),


## ----model-illustration-plot, fig.cap = "(a,b) Model illustrations. (a) In the psychophysical model of moral decision making, utilitarian choices reflect decisions with low internal noise ($w$). Each line represents a different value of $w$ (see legend). (b) Changing the attentional weight of the victims ($\\alpha$) changes the threshold at which sacrificial decisions become acceptable. Each line represents a different value of $\\alpha$ (see legend). (c,d) Illustration of loss aversion as a consequence of magnitude processing. (c) Subjective utility as a function of relative gains or losses with respect to an endowment, for different values of the uncertainty parameter $w$. The S-shaped curve emerges naturally from a psychometric function, while the asymmetric values of utilities of gain and losses emerges from their asymetric Weber ratios. Dotted blue lines mark the utility values at a change of $\\pm 50\\%$, illustrating that the absolute utility of a $50\\%$ loss exceeds that of a $50\\%$ gain. (d) Ratio of absolute utilities for losses vs.\\ gains, $\\frac{\\left|\\text{Utility}_{\\text{Loss}}\\right|}{\\left|\\text{Utility}_{\\text{Gain}}\\right|}$, as a function of relative change. Values greater than 1 indicate loss aversion. Note that loss aversion exceeds 1 for all values of $w$ and increases with the size of the change.", fig.width = 9, fig.height=10----

ggpubr::ggarrange(
    
    plot.utilitarian.deontological,
    ggpubr::ggarrange(
        plot.criterion.shift +
            ggplot2::theme(
                legend.position = "right",
                legend.text = ggplot2::element_text(
                    size = 9,
                    angle = 0
                ),
                legend.title = ggplot2::element_text(
                    size = 9,
                    angle = 90
                )
            ) +
            ggplot2::guides(
                colour = ggplot2::guide_legend(title.position = "left"),
                linetype = ggplot2::guide_legend(title.position = "left")
            )
    ),
    
    dat_prospect_demo %>% 
        create_prospect_change_value_plot(),
    
    dat_prospect_demo %>% 
        create_prospect_relative_value_plot(),
    
    ncol = 2,
    nrow = 2,
    widths = c(1, 1.5),
    labels = "auto",
    common.legend = TRUE,
    legend = "bottom"
)



## ----print-number-conds, results='hide'----------------------------------------------------------
dat.number.conds.for.table <- dat.number.conds %>%
    dplyr::select(-experimentDesc) %>%
    dplyr::filter(
        # Use the authoritative experiment list from the paper correspondence table
        experimentID %in% dat.moral.numbers.exp.correspondance$experimentID.data,
        # Exp. 3a–3c (2AFC experiments) have a separate table
        !experimentID %in% c("exp2a", "exp2b", "exp2c", "exp2ac", "exp2d"),
        # exp4 excluded: number conditions confirmed identical to exp1b; label shows Exp. 1b/2
        experimentID != "exp4",
        # exp11 excluded: replication of exp10 with identical number conditions; label shows Exp. 5/6
        experimentID != "exp11"
    ) %>%
    # Remove second-option columns (only used in 2AFC experiments) and total column
    dplyr::select(-dplyr::matches("^n_(victims|saved|total)2$|^ratio2$|^n_total$")) %>%
    format_exp_info() %>%
    # Merge labels for experiments that share identical number conditions.
    # exp1b (Exp. 1b) and exp4 (Exp. 2) use the same number conditions file.
    # exp10 (Exp. 5) and exp11 (Exp. 6) use the same conditions (exp11 replicates exp10).
    dplyr::mutate(experimentID = dplyr::recode(experimentID,
                                               "Exp. 1b" = "Exp. 1b/2",
                                               "Exp. 5"  = "Exp. 5/6"
    )) %>%
    tidyr::unite("experimentID", experimentID, Group, sep = " - ", remove = TRUE, na.rm = TRUE) %>%
    dplyr::arrange(experimentID, ratio, n_victims, .by_group = TRUE) %>%
    dplyr::rename_with(~ stringr::str_remove(.x, "n_")) %>%
    dplyr::rename(beneficiaries = saved)

dat.number.conds.for.table %>%
    dplyr::select(-experimentID) %>%
    knitr::kable(
        caption = "Number conditions for Experiments 1a through 2 and Experiments 4 through 6. Each row shows one trial type. Experiments sharing a row label used identical number conditions: Exp.~1b/2 (Experiments 1b and 2) and Exp.~5/6 (Experiments 5 and 6, the latter being a direct replication of the former). Number conditions for the 2-alternative forced-choice experiments (Exp.~3a--3c) are given in Table~\\ref{tab:print-number-conds-exp2-2}.",
        booktabs = TRUE,
        longtable = TRUE,
        digits = 3
    ) %>%
    kableExtra::add_header_above(c("Number of" = 2, " " = 1), bold = TRUE) %>%
    kableExtra::kable_classic_2() %>%
    # For long table, pack_rows must come last
    # See https://github.com/haozhu233/kableExtra/issues/476
    kableExtra::pack_rows(index = dat.number.conds.for.table %>%
                              make.pack.index(experimentID))


## ----descriptives-print, results='hide'----------------------------------------------------------
dat.moral.numbers.m.across.subj.for.table <- dat.moral.numbers.m.across.subj %>%
    dplyr::filter(
        experimentID %in% dat.moral.numbers.exp.correspondance$experimentID.data |
            stringr::str_detect(experimentID, "\\.vivacityManipulationWorking$"),
        # Exclude asymptote search experiment (AS); 2AFC experiments are not in this data frame
        experimentID != "exp9a",
        blocks == "all"
    ) %>%
    dplyr::select(-blocks) %>%
    format_exp_info() %>%
    dplyr::arrange(experimentID)

dat.moral.numbers.m.across.subj.for.table %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with(".p"), fix_zero_p_values)) %>%
    dplyr::select(-experimentID) %>%
    knitr::kable(
        caption = "Descriptive statistics for all experiments. Each row summarises one number condition. Raw ratings are on a 1--6 acceptability scale; binary ratings reflect the proportion of \"acceptable\" responses. Cohen's $d$ is the standardised difference from the scale midpoint (3.5). For Experiments 4--6, rows labelled (filtered) include only participants for whom the vivacity manipulation was successful.",
        col.names = c(
            "Decision Type", "Vivacity",
            "Ratio", "Victims", "Beneficiaries", "Participants",
            "M", "SE", "p", "Cohen's $d$",
            "M", "SE", "p"
        ),
        booktabs = TRUE,
        longtable = TRUE,
        digits = 3,
        escape = FALSE
    ) %>%
    kableExtra::add_header_above(c(" " = 3, "Number of" = 3, "Raw ratings" = 4, "Binary ratings" = 3), bold = TRUE) %>%
    kableExtra::kable_classic_2() %>%
    kableExtra::kable_styling(
        font_size = 7,
        latex_options = c(
            "hold_position",
            "striped"
        )
    ) %>%
    # For long table, pack_rows must come last
    # See https://github.com/haozhu233/kableExtra/issues/476
    kableExtra::pack_rows(index = dat.moral.numbers.m.across.subj.for.table %>%
                              make.pack.index(experimentID))


## ----ratio-vs-n-victims-glmer-bin-calculate------------------------------------------------------
# * Model
# - acceptability ~ number_killed + (1|participant) + (1|scenario)

moral.numbers.ratio.vs.n.victims.bin.lmer1 <- glmer(
    rating.bin ~ ratio + n.victims.Z +
        (1 | ResponseId) + (1 | scenarioID),
    control = glmerControl(optimizer = "bobyqa"),
    family = "binomial",
    data = dat.moral.numbers %>%
        dplyr::filter(stringr::str_starts(experimentID, "exp1(b)*$")) %>%
        dplyr::mutate(n.victims.Z = scale(n.victims))
)

# moral.numbers.ratio.vs.n.victims.bin.lmer2 <- stats::update(
#   moral.numbers.ratio.vs.n.victims.bin.lmer1,
#   ~ . - (1 | scenarioID)
# )

# scenarioID contributes to likelihood, so keep it
# anova (
#     moral.numbers.ratio.vs.n.victims.bin.lmer1,
#     moral.numbers.ratio.vs.n.victims.bin.lmer2)

moral.numbers.ratio.vs.n.victims.bin.lmer1.ratio.only <- stats::update(
    moral.numbers.ratio.vs.n.victims.bin.lmer1,
    ~ . - n.victims.Z
)

moral.numbers.ratio.vs.n.victims.bin.lmer1.n.only <- stats::update(
    moral.numbers.ratio.vs.n.victims.bin.lmer1,
    ~ . - ratio
)





## ----ratio-vs-n-net-saved-glmer-bin-calculate----------------------------------------------------
# * Model
# - acceptability ~ net number saved + (1|participant) + (1|scenario)

moral.numbers.ratio.vs.n.net.saved.bin.lmer1 <- lme4::glmer(
    rating.bin ~ ratio + scale(n.net.saved) +
        (1 | ResponseId) + (1 | scenarioID),
    control = glmerControl(optimizer = "bobyqa", optCtrl = list(maxfun = 100000)),
    family = "binomial",
    data = dat.moral.numbers %>%
        dplyr::filter(stringr::str_starts(experimentID, "exp1(b)*$"))
)

# moral.numbers.ratio.vs.n.net.saved.bin.lmer2 <- stats::update(
#   moral.numbers.ratio.vs.n.net.saved.bin.lmer1,
#   ~ . - (1 | scenarioID)
# )

# scenarioID contributes to likelihood, so keep it
# anova (
#     moral.numbers.ratio.vs.n.net.saved.bin.lmer1,
#     moral.numbers.ratio.vs.n.net.saved.bin.lmer2)

moral.numbers.ratio.vs.n.net.saved.bin.lmer1.ratio.only <- stats::update(
    moral.numbers.ratio.vs.n.net.saved.bin.lmer1,
    ~ . - scale(n.net.saved)
)

moral.numbers.ratio.vs.n.net.saved.bin.lmer1.n.only <- stats::update(
    moral.numbers.ratio.vs.n.net.saved.bin.lmer1,
    ~ . - ratio
)




## ----glmer-print, results = 'hide'---------------------------------------------------------------

list(
    "Weber ratio vs. harm: Two fixed effect model"  = 
        moral.numbers.ratio.vs.n.victims.bin.lmer1,
    "Weber ratio vs. harm: One fixed effect model (ratio only)" =
        moral.numbers.ratio.vs.n.victims.bin.lmer1.ratio.only,
    "Weber ratio vs. harm: One fixed effect model (harm only)" =
        moral.numbers.ratio.vs.n.victims.bin.lmer1.n.only,
    
    "Weber ratio vs. utility: Two fixed effect model" =
        moral.numbers.ratio.vs.n.net.saved.bin.lmer1,
    "Weber ratio vs. utility: One fixed effect model (ratio only)" =
        moral.numbers.ratio.vs.n.net.saved.bin.lmer1.ratio.only,
    "Weber ratio vs. utility: One fixed effect model (utility only)" =
        moral.numbers.ratio.vs.n.net.saved.bin.lmer1.n.only
) %>%
    purrr::map(\(x) extract.results.from.binary.model(x, output.wide = TRUE)) %>% 
    purrr::list_rbind(names_to = "lmer.label") %>% 
    dplyr::select(-t_log, -p_log) %>%
    rename.terms(term) %>%
    dplyr::mutate(
        term = stringr::str_wrap(term, 25),
        dplyr::across(
            dplyr::where(is.numeric),
            ~ ifelse(.x > 1,
                     round(.x, 2),
                     signif(.x, 3)
            )
        )
    ) %>%
    fix_zero_p_values(p_or) %>%
    kable.packed(
        "lmer.label",
        caption = "Full results of binomial GLMMs predicting binarized acceptability ratings (acceptable vs. not acceptable) from Experiments 1a and 1b combined, with random intercepts for participants and scenarios. Top three panels: models comparing effects of the beneficiary:victim Weber ratio and harm (number of victims, $Z$-scored) as fixed effects -- a model with both fixed effects (Weber ratio + harm), a model with Weber ratios as the sole fixed effect (ratio only), and a model with harm as the sole fixed effect (harm only). Bottom three panels: equivalent models replacing harm with utility (net number of lives saved). Note that neither harm nor utility contributed to the model likelihood in the two fixed effect models.",
        col.names = c("Effect", "Estimate", "SE", "CI", "Estimate", "SE", "CI", "t", "p"),
        escape = FALSE,
        booktabs = TRUE
    ) %>%
    kableExtra::add_header_above(c(" " = 1, "Log-odds space" = 3, "Odds-ratio space" = 3, " " = 2)) %>%
    kableExtra::kable_styling(
        bootstrap_options = "striped",
        latex_options = c("striped", "hold_position", "scale_down")
    ) %>%
    kableExtra::kable_classic_2()


## ----exp1-model-comp-calculate-------------------------------------------------------------------
l.exps.for.comp.exp1 <- list(
    Ratio = list(
        mod.target = moral.numbers.ratio.vs.n.victims.bin.lmer1.ratio.only,
        # Full model with an additional predictor to which the single predictor model will be compared
        mod.full = moral.numbers.ratio.vs.n.victims.bin.lmer1,
        added.predictor = "n.victims"
    ),
    Ratio = list(
        mod.target = moral.numbers.ratio.vs.n.net.saved.bin.lmer1.ratio.only,
        mod.full = moral.numbers.ratio.vs.n.net.saved.bin.lmer1,
        added.predictor = "n.net.saved"
    ),
    n.victims = list(
        mod.target = moral.numbers.ratio.vs.n.victims.bin.lmer1.n.only,
        mod.full = moral.numbers.ratio.vs.n.victims.bin.lmer1,
        added.predictor = "ratio"
    ),
    n.net.saved = list(
        mod.target = moral.numbers.ratio.vs.n.net.saved.bin.lmer1.n.only,
        mod.full = moral.numbers.ratio.vs.n.net.saved.bin.lmer1,
        added.predictor = "ratio"
    )
)


dat.moral.numbers.model.comp.exp1 <- l.exps.for.comp.exp1 %>%
    generate_model_comp(rename_stuff_for_print = TRUE)


## ----exp1-model-comp-print, include=FALSE--------------------------------------------------------
dat.moral.numbers.model.comp.exp1 %>%
    dplyr::mutate(dplyr::across(
        dplyr::where(is.character),
        ~ stringr::str_replace(.x, "#", "\\\\#")
    )) %>%
    dplyr::mutate(
        dplyr::across(
            dplyr::matches("\\$p\\$"), 
            fix_zero_p_values
        )
    ) %>%
    
    knitr::kable(
        #' latex',
        caption = "Model comparisons for Experiment 1. AICc: Akaike Information Criterion with small sample correction. $\\rho_{ZA}$: \\cite{Zheng2000} correlation between fitted and actual values. Other columns: Reduction in AICc and the associated p-value when a second predictor is added to a generalized linear mixed model. For example, in the first row, we compare a model with Ratio as the sole fixed effect to models also including the number of victims (Number of Victims columns) or the utility (Utility columns). Last row: We also fit to a model with the proportion of saved as the sole predictor.",
        booktabs = TRUE,
        escape = FALSE,
        col.names =
            c(
                "Predictor", "AICc", "$\\rho_{ZA}$",
                stringr::str_remove_all(names(.)[-(1:3)], "_.*$")
            )
    ) %>%
    kableExtra::add_header_above(
        c(
            " " = 3,
            dat.moral.numbers.model.comp.exp1 %>%
                dplyr::select(dplyr::contains("_")) %>%
                names() %>%
                stringr::str_remove_all("^.*_") %>%
                stringr::str_to_title() %>% 
                make.pack.index()
        ),
        bold = TRUE
    ) %>%
    kableExtra::add_header_above(c(" " = 3, "Change in model fit due to addition of" = 6),
                                 bold = TRUE, escape = FALSE
    ) %>%
    kableExtra::kable_styling(latex_options = c("scale_down")) %>%
    # "hold_position",
    kableExtra::kable_classic_2()


## ----fit-prepare-data----------------------------------------------------------------------------
# For overall fit
dat.moral.numbers.m.across.subj.for.fit <- dat.moral.numbers.m.across.subj %>%
    # Just in case
    dplyr::ungroup() %>%
    # Weed out experiments with 2AFC responses
    dplyr::filter(!stringr::str_starts(experimentID, "exp[23]")) %>%
    # combine exp1 and 1b, might need to combine other experiments as well (e.g., exp 5)
    # We need to explicitly select the experiments to avoid excluding experiment 10
    dplyr::filter(!(experimentID %in% c("exp1", "exp1b"))) %>%
    dplyr::mutate(experimentID = stringr::str_replace(experimentID, "exp1-combined", "exp1"))
# dplyr::filter(!(experimentID %in% c("exp5", "exp5b")))

# For bootstrap fit
dat.moral.numbers.for.bootstrap.fit <- dplyr::bind_rows(
    # Here we collapse across all blocks (in experiments with multiple blocks)
    dat.moral.numbers %>%
        # Just in case
        dplyr::ungroup() %>%
        # Remove severity questions for experiments that have them
        dplyr::filter(question == "acceptability") %>%
        # Weed out experiments with 2AFC responses
        dplyr::filter(!stringr::str_starts(experimentID, "exp[23]")) %>%
        # combine exp1 and 1b, might need to combine other experiments as well
        # We need to explicitly select the experiments to avoid excluding experiment 10
        dplyr::mutate(experimentID = stringr::str_replace(experimentID, "exp1b*", "exp1")) %>%
        dplyr::mutate(blocks = "all", .after = "experimentID"),
    
    # First blocks only
    dat.moral.numbers %>%
        # Just in case
        dplyr::ungroup() %>%
        dplyr::filter(experimentID == "exp4") %>%
        # Remove severity questions for experiments that have them
        dplyr::filter(question == "acceptability") %>%
        dplyr::filter(first.scenario.type == decisionType) %>%
        dplyr::mutate(blocks = "first", .after = "experimentID"),
    
    dat.moral.numbers %>%
        # Just in case
        dplyr::ungroup() %>%
        # The experiment list below could be replaced with filter(!is.na(vivacity)) if re-run
        dplyr::filter(
            experimentID %in% c("exp5", "exp6", "exp7") |
                stringr::str_detect(experimentID, "^exp(8|10|11)")
        ) %>%
        # Remove severity questions for experiments that have them
        dplyr::filter(question == "acceptability") %>%
        dplyr::filter(first.scenario.type == vivacity) %>%
        dplyr::mutate(blocks = "first", .after = "experimentID"),
    
    # First block of Experiment 5 and 5b combined
    dat.moral.numbers %>%
        # Just in case
        dplyr::ungroup() %>%
        dplyr::filter(stringr::str_starts(experimentID, "exp5")) %>%
        # Remove severity questions for experiments that have them
        dplyr::filter(question == "acceptability") %>%
        dplyr::mutate(experimentID = stringr::str_replace(experimentID, "^exp5.*", "exp5ab")) %>%
        dplyr::filter(first.scenario.type == vivacity) %>%
        dplyr::mutate(blocks = "first", .after = "experimentID")
) %>%
    dplyr::arrange(experimentID, blocks)


## ----fit-overall-calculate-----------------------------------------------------------------------
dat.moral.numbers.overall.fit.1param <- dat.moral.numbers.m.across.subj.for.fit %>%
    dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
    dplyr::group_modify(~ tryCatch(
        minpack.lm::nlsLM(
            rating.bin.M ~ acceptability.fnc(
                w,
                n.saved, n.victims
            ),
            data = .x,
            # This is a list
            start = FIT_PARAMS$start["w"],
            # nlsLM specific options
            # These are vectors
            lower = FIT_PARAMS$lower["w"],
            upper = FIT_PARAMS$upper["w"]
        ) %>%
            broom::tidy(),
        error = function(e) data.frame(w = NA)
    )) %>%
    # Just in case
    dplyr::ungroup()

dat.moral.numbers.overall.fit.2param <- dat.moral.numbers.m.across.subj.for.fit %>%
    dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
    dplyr::group_modify(~ tryCatch(
        minpack.lm::nlsLM(
            rating.bin.M ~ acceptability.fnc(w,
                                             n.saved, n.victims,
                                             a = a
            ),
            data = .x,
            # This is a list
            start = FIT_PARAMS$start,
            # nlsLM specific options
            # These are vectors
            lower = FIT_PARAMS$lower,
            upper = FIT_PARAMS$upper
        ) %>%
            broom::tidy(),
        error = function(e) data.frame(w = NA, a = NA)
    )) %>%
    # Just in case
    dplyr::ungroup()


## ----fit-linearized-calculate--------------------------------------------------------------------
dat.moral.numbers.overall.fit.1param.linearized <- dat.moral.numbers.m.across.subj.for.fit %>%
    dplyr::rename(rating.bin = rating.bin.M) %>%
    dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
    dplyr::group_modify(~ tryCatch(
        get.weber.ratio.linearized(.x),
        error = function(e) data.frame(w = NA)
    )) %>%
    # Just in case
    dplyr::ungroup()

dat.moral.numbers.overall.fit.2param.linearized <- dat.moral.numbers.m.across.subj.for.fit %>%
    dplyr::rename(rating.bin = rating.bin.M) %>%
    dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
    dplyr::group_modify(~ tryCatch(
        get.weber.ratio.linearized(.x, fit.w = TRUE, fit.a = TRUE),
        error = function(e) data.frame(w = NA)
    )) %>%
    # Just in case
    dplyr::ungroup()




## ----fit-bootstrap-calculate---------------------------------------------------------------------
if (RECALCULATE_EVERYTHING) {
    # This will take about 10 min for 1000 samples, so presumably 100 min for 10000.
    
    gc() # Clean up memory just in case
    
    # Boot library is annoying for grouped data, so create your own bootstrap
    
    # bootstrap.weber.ratio.old: 29.15 s for 100 samples => 7min13s for 1000 samples
    # bootstrap.weber.ratio.new: 33.08 s for 100 samples => 7min25s for 1000 samples
    # tic()
    warning("I'm about to generate ", N_BOOTSTRAP, " bootstrap samples. This will take about 8 min.")
    dat.moral.numbers.bootstrap.1param <- furrr::future_imap_dfr(1:(N_BOOTSTRAP / 1),
                                                                 ~ dat.moral.numbers.for.bootstrap.fit %>%
                                                                     dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
                                                                     dplyr::group_modify(~ bootstrap.weber.ratio(.x, fit.a = FALSE, fit.w = TRUE)),
                                                                 .options = furrr_options(
                                                                     seed = TRUE,
                                                                     stdout = !RECALCULATE_EVERYTHING
                                                                 )
    )
    # toc()
    
    dat.moral.numbers.bootstrap.1param.summary <-
        dat.moral.numbers.bootstrap.1param %>%
        dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
        summarize_bootstrap_samples()
    #    # wilcox.test(w  ~ vivacity, data = .)
    
    
    warning("I'm about to generate ", N_BOOTSTRAP, " bootstrap samples. This will take about 8 min.")
    dat.moral.numbers.bootstrap.2param <- furrr::future_imap_dfr(1:(N_BOOTSTRAP / 1),
                                                                 ~ dat.moral.numbers.for.bootstrap.fit %>%
                                                                     dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
                                                                     dplyr::group_modify(~ bootstrap.weber.ratio(.x, fit.a = TRUE, fit.w = TRUE)),
                                                                 .options = furrr_options(
                                                                     seed = TRUE,
                                                                     stdout = !RECALCULATE_EVERYTHING
                                                                 )
    )
    
    dat.moral.numbers.bootstrap.2param.summary <-
        dat.moral.numbers.bootstrap.2param %>%
        dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
        summarize_bootstrap_samples()
    #    # wilcox.test(w  ~ vivacity, data = .)
    
    # Save data
    save(
        dat.moral.numbers.bootstrap.1param,
        dat.moral.numbers.bootstrap.1param.summary,
        dat.moral.numbers.bootstrap.2param,
        dat.moral.numbers.bootstrap.2param.summary,
        file = here::here(
            OUTPUT_DIR, 
            "bootstrap.fits.RData"
        )
    )
    
    gc() # Clean up memory just in case
} else {
    # Just load the data
    load(here::here(
        OUTPUT_DIR,
        "bootstrap.fits.RData"
    )
    )
}


## ----exp1-model-comparisons-plot-bin-prepare-----------------------------------------------------
l.exp1.model.comp.plots <- dat.moral.numbers %>%
    dplyr::filter(stringr::str_starts(experimentID, "exp1(b)*$")) %>%
    create.predictor.comparison.plot(
        facet.var = NULL,
        dat.fit = get_dat_fit_for_plot(experimentID == "exp1", blocks == "all",
                                       fit_type = FIT_FOR_FIGURE, n_fit_params = 1
        ),
        value.var = rating.bin,
        col.var = NULL,
        ylab = "Acceptability",
        legend = "bottom",
        add.fit = TRUE,
        return.plot = FALSE,
        add.p.saved.to.list = TRUE
    )


## ----exp1-model-comparisons-plot-bin, fig.cap = "Acceptability against (a) the Weber ratio, (b) the probability of being saved, (c) the number of victims and (d) the net number of saved. Error bars represent 95\\% bootstrap confidence intervals. While the ratings seem to show a sigmoid dependency on the Weber ratio and a linear dependency on the probability of saving, there appears to be no clear relation between the acceptability ratings and the number of victims or the net number of saved.", eval = SHOW_INDIVIDUAL_PLOTS----
# ggpubr::ggarrange(
#     plotlist = l.exp1.model.comp.plots,
#     ncol = 2, nrow = 2, labels = "auto"
# ) %>%
#     ggpubr::annotate_figure(top = "Comparison of predictors of acceptability - Exp. 1a & 1b (binary ratings)")


## ----exp4-model-comparisons-plot-bin-prepare-----------------------------------------------------
l.exp4.model.comp.plots <- dat.moral.numbers %>%
    dplyr::filter(experimentID == "exp4") %>%
    dplyr::mutate(decisionType = stringr::str_to_title(decisionType)) %>%
    create.predictor.comparison.plot(
        facet.var = NULL,
        dat.fit = get_dat_fit_for_plot(experimentID == "exp4", blocks == "all",
                                       fit_type = FIT_FOR_FIGURE, n_fit_params = 1
        ),
        value.var = rating.bin,
        col.var = decisionType,
        ylab = "Acceptability",
        legend = "bottom",
        add.fit = TRUE,
        return.plot = FALSE,
        add.p.saved.to.list = TRUE
    )


## ----exp4-model-comparisons-plot-bin, fig.cap = "Acceptability in Experiment 2 against (a) the Weber ratio, (b) the probability of being saved, (c) the number of victims and (d) the net number of saved. Error bars represent 95\\% bootstrap confidence intervals. Model fits are based on on the average response of all participants. While the ratings seem to show a sigmoid dependency on the Weber ratio and a linear dependency on the probability of saving, there appears to be no clear relation between the acceptability ratings and the number of victims or the net number of saved.", eval = SHOW_INDIVIDUAL_PLOTS----
# ggpubr::ggarrange(
#     plotlist = l.exp4.model.comp.plots,
#     ncol = 2, nrow = 2, labels = "auto",
#     common.legend = TRUE
# ) %>%
#     ggpubr::annotate_figure(top = "Comparison of predictors of acceptability - Exp. 2 (binary ratings)")


## ----model-comparisons-plot-bin, fig.height = 7, fig.width = 10, fig.cap = "Binarized acceptability ratings as a function of (left column) the \\emph{beneficiary:victim} Weber ratio, (center column) the number of victims (harm), and (right column) the net number of survivors (utility). Model fits are based on the average response of all participants. Error bars represent 95\\% bootstrap confidence intervals. Acceptability ratings show a clear sigmoid dependency on the \\emph{beneficiary:victim} Weber ratio, but no systematic relationship with harm or utility. (a) Experiment 1, including only moral choices. (b) Experiment 2, including both moral and economic choices. Economic choices yielded a lower noise ($w$) parameter and thus more utilitarian responses."----

list(
    "a   Experiment 1" = ggpubr::ggarrange(
        plotlist = l.exp1.model.comp.plots %>%
            purrr::discard_at("plot.by.p.saved") %>%
            purrr::map(\(p) p + ggplot2::theme(plot.background = ggplot2::element_rect(colour = NA, fill = NA))),
        ncol = 3,
        legend = "none"
    ),
    "b   Experiment 2" = ggpubr::ggarrange(
        plotlist = l.exp4.model.comp.plots %>%
            purrr::discard_at("plot.by.p.saved") %>%
            purrr::map(\(p) p + ggplot2::theme(plot.background = ggplot2::element_rect(colour = NA, fill = NA))),
        ncol = 3,
        common.legend = TRUE, legend = "bottom"
    )
) %>%
    purrr::imap(\(row, name) {
        p <- ggpubr::annotate_figure(
            row,
            top = ggpubr::text_grob(name, face = "bold", hjust = 0, x = 0.01)
        )
        cowplot::ggdraw(p) +
            ggplot2::theme(plot.background = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1))
    }) %>%
    ggpubr::ggarrange(
        plotlist = .,
        ncol = 1,
        nrow = 2
    )


## ----exp4-add-Z-values-to-bootstrap-fit----------------------------------------------------------
# Z values for the w  parameters for the economic version with respect to the moral version

l.exp4.Z.economic.1param <- dat.moral.numbers.bootstrap.1param.summary %>%
    dplyr::filter(experimentID == "exp4", blocks == "all") %>%
    dplyr::ungroup() %>%
    make.Z.value.against.control(group = decisionType, controlCond = "moral")



## ----exp4-fits-bootstrap-w-plot, fig.cap = "Bootstrap samples of the $w$ parameter in Experiment 2 (moral vs. economic decisions). Each violin shows the distribution of bootstrap estimates.", include=SHOW_INDIVIDUAL_PLOTS----

p.moral.numbers.bootstrap.1param.exp4 <- dat.moral.numbers.bootstrap.1param %>%
    dplyr::filter(experimentID == "exp4", blocks == "all", !is.na(w)) %>%
    dplyr::mutate(decisionType = stringr::str_to_title(decisionType)) %>%
    format_exp_info() %>% 
    # Need two line facet labels for combining the figure with that of Exp 3
    dplyr::mutate(experimentID = str_c(experimentID, "\n")) %>% 
    ggplot2::ggplot(ggplot2::aes(x = decisionType, y = w)) %>%
    violin_plot_template(yintercept = NULL, add.dot.plot = FALSE) +
    ggplot2::labs(
        x = "Decision type",
        y = latex2exp::TeX("$w$")
    ) +
    ggplot2::facet_wrap(~experimentID, scales = "free_y")

p.moral.numbers.bootstrap.1param.exp4


## ----print-number-conds-exp2, results = 'hide'---------------------------------------------------
dat.moral.numbers.number.cond.exp2.for.table <- dplyr::bind_rows(
    read.csv(
        here::here(            
            EXPERIMENT_DIR,
            "experiment2_3", 
            "final", 
            "number_conditions", 
            "number_conds_2x2_choices_ratio_vs_victims.csv"
        ),
        header = TRUE
    ) %>%
        dplyr::mutate(comparisonType = "Exp. 3a: Ratio vs. # victims"),
    read.csv(
        here::here(            
            EXPERIMENT_DIR,
            "experiment2_3", 
            "final", 
            "number_conditions", 
            "number_conds_2x2_choices_ratio_vs_utility.csv"
        ),
        header = TRUE
    ) %>%
        dplyr::mutate(comparisonType = "Exp. 3b: Ratio vs. utility"),
    read.csv(
        here::here(            
            EXPERIMENT_DIR,
            "experiment2_3", 
            "final", 
            "number_conditions", 
            "number_conds_2x2_choices_ratio_vs_utility_1.5utility_3xvictims.csv"
        ),
        header = TRUE
    )  %>%
        dplyr::mutate(comparisonType = "Exp. 3c: Ratio vs. utility - less extreme contrasts")
) %>%
    dplyr::mutate(ratio1 = n_saved1 / n_victims1, .after = n_saved1) %>%
    dplyr::mutate(ratio2 = n_saved2 / n_victims2, .after = n_saved2) %>%
    dplyr::select(-dplyr::matches("total"))

dat.moral.numbers.number.cond.exp2.for.table %>%
    dplyr::select(-comparisonType) %>%
    knitr::kable(
        caption = "Number conditions for the 2-alternative forced-choice experiments (Experiments 3a--3c). Each row shows one trial type. In each trial, participants chose between a high-ratio option and a low-ratio option. The high-ratio option always had a higher beneficiary:victim ratio than the low-ratio option. In Experiment 3a, the low-ratio option had fewer victims (harm contrast); in Experiments 3b and 3c, it had higher net utility (utility contrast).",
        col.names = rep(c("\\# victims", "\\# beneficiaries", "ratio"), 2),
        booktabs = TRUE,
        longtable = TRUE,
        escape = FALSE
    ) %>%
    # reduce(1:6, ~ column_spec(.x, .y, width = "2.5cm"), .init = .) %>%
    kableExtra::kable_styling(
        bootstrap_options = "striped",
        latex_options = c("striped", "hold_position", "repeat_header"), # "scale_down"),
        full_width = FALSE
    ) %>%
    kableExtra::add_header_above(c("High ratio option" = 3, "Low ratio option" = 3), bold = TRUE) %>%
    kableExtra::kable_classic_2() %>%
    # For long table, pack_rows must come last
    # See https://github.com/haozhu233/kableExtra/issues/476
    kableExtra::pack_rows(index = dat.moral.numbers.number.cond.exp2.for.table %>%
                              make.pack.index(comparisonType))


## ----exp2-make-by-subj-averages------------------------------------------------------------------
# Overall
dat.moral.numbers.exp2.m <- dat.moral.numbers %>%
    dplyr::filter(stringr::str_starts(experimentID, "exp2")) %>%
    # Combining Experiments 2a and 2c as they are replications of one another
    combine_exps("exp2[ac]") %>%
    tidyr::pivot_longer(dplyr::starts_with("rating"),
                        names_to = "measure",
                        values_to = "rating"
    ) %>%
    dplyr::mutate(measure = stringr::str_remove(measure, "rating.")) %>%
    dplyr::group_by(experimentID, ResponseId, first.scenario.type, comparisonType, decisionType, ratioOrder, measure) %>%
    dplyr::summarize(rating = mean(rating), .groups = "drop") %>%
    dplyr::mutate(chance.level = ifelse(measure == "bin",
                                        .5,
                                        3.5
    ))

# By block
dat.moral.numbers.exp2.by.block.m <- dat.moral.numbers %>%
    dplyr::filter(stringr::str_starts(experimentID, "exp2")) %>%
    # Combining Experiments 2a and 2c as they are replications of one another
    combine_exps("exp2[ac]") %>%
    tidyr::pivot_longer(dplyr::starts_with("rating"),
                        names_to = "measure",
                        values_to = "rating"
    ) %>%
    dplyr::mutate(measure = stringr::str_remove(measure, "rating.")) %>%
    dplyr::group_by(experimentID, ResponseId, block, first.scenario.type, comparisonType, decisionType, ratioOrder, measure) %>%
    dplyr::summarize(rating = mean(rating), .groups = "drop") %>%
    dplyr::mutate(chance.level = ifelse(measure == "bin",
                                        .5,
                                        3.5
    ))


## ----exp2-plot-bin, fig.width = 8.5, fig.cap = "Results of Experiment 3 for responses recoded in terms of a binary acceptable/unacceptable decision. The dot, error bars and violin represent the sample average, 95\\% bootstrap confidence intervals and the distribution of the individual responses, respectively. Orange circles represent individual participants.", include=SHOW_INDIVIDUAL_PLOTS----

p.moral.numbers.exp2.m <- dat.moral.numbers.exp2.m %>%
    dplyr::filter(measure == "bin") %>%
    format_exp_info(wrap = TRUE) %>%
    dplyr::mutate(experimentID = stringr::str_to_sentence(experimentID)) %>%
    dplyr::mutate(decisionType = stringr::str_to_title(decisionType)) %>%
    ggplot2::ggplot(ggplot2::aes(x = decisionType, y = rating)) %>%
    violin_plot_template(yintercept = 0.5, dotsize = 0.2) +
    ggplot2::scale_y_continuous("Rating in favor of high ratio option") +
    ggplot2::labs(x = "Decision type") +
    ggplot2::facet_wrap(~experimentID, scales = "free_y")

p.moral.numbers.exp2.m 


## ----exp2-plot-raw, fig.width = 8.5, fig.cap = "Results of Experiment 3 for raw ratings on a 6-point scale. The dot, error bars and violin represent the sample average, 95\\% bootstrap confidence intervals and the distribution of the individual responses, respectively. Orange circles represent individual participants. The horizontal line indicates the chance level (3.5).", eval = FALSE----
# dat.moral.numbers.exp2.m %>%
#     dplyr::filter(measure == "raw") %>%
#     format_exp_info(wrap = TRUE) %>%
#     dplyr::mutate(decisionType = stringr::str_to_title(decisionType)) %>%
#     ggplot2::ggplot(ggplot2::aes(x = decisionType, y = rating)) %>%
#     violin_plot_template(yintercept = 3.5, dotsize = 0.2) +
#     ggplot2::scale_y_continuous("Rating in favor of high ratio option") +
#     ggplot2::labs(x = "Decision type") +
#     ggplot2::facet_wrap(~experimentID, scales = "free_y")


## ----exp4-fits-bootstrap-w-exp2-plot, fig.cap = "(a) Bootstrap samples of the $w$ (noise) parameter in Experiment 2 (moral vs. economic decisions). Each violin shows the distribution of bootstrap estimates. Estimates in the economic condition are substantially less noisy than in the moral condition. (b) Results of Experiment 3 for responses recoded in terms of a binary acceptable/unacceptable decision. The dots, error bars and violins represent the sample averages, 95\\% bootstrap confidence intervals and the distributions of the individual responses, respectively. Orange circles represent individual participants. In moral dilemmas, participants favor beneficiary:victim ratios over both harm (number of victims) and utility (net number of survivors). In economic dilemmas, participants still favor beneficiary:victim ratios over harm, but favor utility over beneficiary:victim ratios.", fig.width = 8----

list(
    p.moral.numbers.bootstrap.1param.exp4,
    p.moral.numbers.exp2.m
) %>% 
    purrr::map(\(p) p + ggplot2::labs(x = NULL)) %>%
    ggpubr::ggarrange(
        plotlist = .,
        ncol = 2,
        labels = "auto",
        widths = c(1, 3)
        #legend = "bottom",
        #common.legend = TRUE
    ) 



## ----exp2-descriptives-prepare-------------------------------------------------------------------

dat.moral.numbers.exp2.descriptives <- dplyr::bind_rows(
    dat.moral.numbers.exp2.m %>%
        dplyr::mutate(block = "Both"),
    dat.moral.numbers.exp2.by.block.m
) %>%
    dplyr::group_by(measure, experimentID, comparisonType, block, decisionType) %>%
    dplyr::summarize(
        N = dplyr::n(),
        M = mean(rating),
        SE = se(rating),
        "Cohen's d" = (mean(rating) - unique(chance.level)) / sd(rating),
        p_wilcox = wilcox.p(rating, mu = unique(chance.level)),
        N_above_chance_level = sum(rating > unique(chance.level), na.rm = TRUE),
        # Note that we don't exclude ties. This makes the test more conservative
        N_binom_threshold = get.min.number.of.correct.trials.by.binom.test(
            N, # Ties not excluded
            alternative = "greater"
        ),
        P_above_chance_level = N_above_chance_level / N, # Ties not excluded
        p_binom = binom.test(
            N_above_chance_level,
            N, # Ties not excluded
            alternative = "two.sided"
        )$p.value / 2,
        .groups = "drop"
    )




## ----exp2-descriptives-prepare-count-stat-thresholds---------------------------------------------

moral.numbers.exp2.binom.sig.thresholds <-  dat.moral.numbers.exp2.descriptives %>% 
    dplyr::filter(measure == "bin") %>%
    dplyr::mutate(sig.threshold = 
                      round(N_binom_threshold / N, 3)
    ) %>% 
    dplyr::select(N, sig.threshold ) %>%
    dplyr::distinct() %>% 
    dplyr::mutate(N = stringr::str_c("N = ", N)) %>%
    tidyr::unite(sig.threshold, sep = ": ") %>%
    dplyr::pull(sig.threshold) %>%
    knitr::combine_words()



## ----exp2-descriptives-print, results = 'hide'---------------------------------------------------



dat.moral.numbers.exp2.descriptives %>%
    dplyr::select(-N_above_chance_level, -P_above_chance_level, -N_binom_threshold) %>%
    format_exp_info() %>%
    dplyr::mutate(experimentID = stringr::str_replace(experimentID, "^exp", "Exp. ")) %>%
    dplyr::mutate(measure = dplyr::case_when(
        measure == "bin" ~ "Binarized ratings",
        measure == "raw" ~ "Raw ratings",
        TRUE ~ measure
    )) %>%
    dplyr::filter(block == "Both") %>% 
    dplyr::select(-block) %>% 
    dplyr::mutate(
        dplyr::across(
            dplyr::starts_with("p_"),
            fix_zero_p_values
        )
    ) %>% 
    dplyr::arrange(measure, experimentID, desc(decisionType)) %>%
    dplyr::rename_with(replace_default_column_labels) %>%
    tidyr::unite(experimentID.measure, Experiment, measure, sep = " - ") %>%
    kable.packed("experimentID.measure",
                 caption = "Descriptives for Experiment 3. Binarized ratings are derived from the 6-point scale by converting values below 3.5 to 0 and values above 3.5 to 1. $M$ is the mean across participants (chance level = 0.5 for binarized ratings, 3.5 for raw ratings). $SE$ = standard error. $p$ (Wilcoxon) is a two-tailed Wilcoxon signed-rank test against chance. $p$ (binomial) tests whether the number of participants whose mean rating exceeded chance is significantly greater or smaller than expected by chance (one-tailed binomial test, significant in either direction); participants at exactly the chance level were counted against the pattern rather than excluded, making the test conservative.",
                 booktabs = TRUE,
                 longtable = FALSE
    ) %>%
    kableExtra::kable_styling(
        bootstrap_options = "striped",
        latex_options = c("striped", "hold_position", "repeat_header"), # , "scale_down"),
        full_width = FALSE
    ) %>%
    kableExtra::column_spec(2, width = "10em") %>%
    kableExtra::add_footnote(label = stringr::str_c("Given the number of participants, the proportion of participants preferring the high-ratio option would be significant by a binomial test when the proportions reach the following thresholds: ", moral.numbers.exp2.binom.sig.thresholds)) %>%
    kableExtra::kable_classic_2()


## ----exp2-lmer-calculate-------------------------------------------------------------------------
# Experiment 2a and c combined
moral.numbers.exp2ac.bin.lmer1 <- lme4::glmer(
    rating.bin ~ decisionType * block +
        (1 | ResponseId) + (1 | scenarioID),
    # control=glmerControl(optimizer="bobyqa"),
    family = "binomial",
    data = dat.moral.numbers %>%
        dplyr::filter(
            stringr::str_detect(experimentID, "exp2[ac]"),
            comparisonType == "ratio_vs_utility"
        ) %>%
        dplyr::mutate(
            decisionType = factor(
                decisionType,
                levels = rev(unique(sort(decisionType)))
            )
        )
)


moral.numbers.exp2ac.bin.lmer2 <- stats::update(
    moral.numbers.exp2ac.bin.lmer1,
    ~ . - (1 | scenarioID)
)


# # Remove double interaction
# moral.numbers.exp2ac.bin.lmer3 <- stats::update(
#   moral.numbers.exp2ac.bin.lmer2,
#   ~ . - decisionType:block
# )
# 
# model.comp.exp2ac.bin <- anova(
#   moral.numbers.exp2ac.bin.lmer1,
#   moral.numbers.exp2ac.bin.lmer2,
#   moral.numbers.exp2ac.bin.lmer3
# )

# Keep model 2 so we can show interaction in table


# Experiment 2d
moral.numbers.exp2d.bin.lmer1 <- lme4::glmer(
    rating.bin ~ decisionType * block +
        (1 | ResponseId) + (1 | scenarioID),
    # control=glmerControl(optimizer="bobyqa"),
    family = "binomial",
    data = dat.moral.numbers %>%
        dplyr::filter(
            stringr::str_detect(experimentID, "exp2d"),
            comparisonType == "ratio_vs_utility"
        ) %>%
        dplyr::mutate(
            decisionType = factor(
                decisionType,
                levels = rev(unique(sort(decisionType)))
            )
        )
)


moral.numbers.exp2d.bin.lmer2 <- stats::update(
    moral.numbers.exp2d.bin.lmer1,
    ~ . - (1 | scenarioID)
)

# # Remove double interaction
# moral.numbers.exp2d.bin.lmer3 <- stats::update(
#   moral.numbers.exp2d.bin.lmer2,
#   ~ . - decisionType:block
# )
# 
# model.comp.exp2d.bin <- anova(
#   moral.numbers.exp2d.bin.lmer1,
#   moral.numbers.exp2d.bin.lmer2,
#   moral.numbers.exp2d.bin.lmer3
# )


# Experiment 2b
moral.numbers.exp2b.bin.lmer1 <- lme4::glmer(
    rating.bin ~ decisionType * block +
        (1 | ResponseId) + (1 | scenarioID),
    # control=glmerControl(optimizer="bobyqa"),
    family = "binomial",
    data = dat.moral.numbers %>%
        dplyr::filter(
            stringr::str_starts(experimentID, "exp2"),
            !stringr::str_detect(comparisonType, "ratio_vs_utility")
        ) %>%
        dplyr::mutate(decisionType = factor(
            decisionType, 
            levels = rev(unique(sort(decisionType)))
        )
        )
)

moral.numbers.exp2b.bin.lmer2 <- stats::update(
    moral.numbers.exp2b.bin.lmer1,
    ~ . - (1 | scenarioID)
)


# # Remove double interaction
# moral.numbers.exp2b.bin.lmer3 <- stats::update(
#   moral.numbers.exp2b.bin.lmer2,
#   ~ . - decisionType:block
# )
# 
# 
# model.comp.exp2b.bin <- anova(
#   moral.numbers.exp2b.bin.lmer1,
#   moral.numbers.exp2b.bin.lmer2,
#   moral.numbers.exp2b.bin.lmer3
# )

# Convergence issue is from moral.numbers.exp2b.bin.lmer3, which we don't use anyhow



## ----exp2-lmer-print, results = 'hide'-----------------------------------------------------------
list(
    "Exp. 3a: Weber ratios vs. harm"          = moral.numbers.exp2b.bin.lmer2,   # exp2b in data
    "Exp. 3b: Weber ratios vs. utility (1)"   = moral.numbers.exp2ac.bin.lmer2,  # exp2a+c combined
    "Exp. 3c: Weber ratios vs. utility (2)"   = moral.numbers.exp2d.bin.lmer2    # exp2d in data
) %>%
    purrr::map(
        \(x) extract.results.from.binary.model(x, output.wide = TRUE)
    ) %>%
    purrr::list_rbind(names_to = "lmer.label") %>% 
    dplyr::select(-t_log, -p_log) %>%
    rename.terms(term) %>%
    dplyr::mutate(
        term = stringr::str_wrap(term, 25),
        dplyr::across(
            dplyr::where(is.numeric),
            ~ ifelse(.x > 1,
                     round(.x, 2),
                     signif(.x, 3)
            )
        )
    ) %>%
    fix_zero_p_values(p_or) %>%
    #highlight_rows_by_p_value(p_col = p_or, cols = -lmer.label, format = "latex") %>%
    kable.packed("lmer.label",
                 caption = "Results in Experiment 3 from generalized linear mixed models with a binomial link function and the fixed factor predictors Domain (moral vs. economic) and Block (first vs. second), their interaction as well as random intercepts for participants. The dependent variable was the endorsement of high-ratio options. Following \\cite{Baayen2008}, we removed all fixed and random predictors that did not contribute to the model likelihood. As a result, we kept only participants as random factors. While it did not contribute to the model likelihood either, we kept the interaction between Domain and Block.",
                 col.names = c("Effect", "Estimate", "SE", "CI", "Estimate", "SE", "CI", "t", "p"),
                 escape = FALSE,
                 booktabs = TRUE
    ) %>%
    kableExtra::add_header_above(c(" " = 1, "Log-odds space" = 3, "Odds-ratio space" = 3, " " = 2)) %>%
    kableExtra::kable_styling(
        bootstrap_options = "striped",
        latex_options = c("striped", "hold_position", "scale_down")
    ) %>%
    # kableExtra::column_spec(1, width = "15em") %>%
    # kableExtra::column_spec(4, width = "4em") %>%
    # kableExtra::column_spec(9, width = "4em") %>%
    kableExtra::kable_classic_2()


## ----find-experiments-with-vivacity-manipulation-------------------------------------------------
l_exp_with_single_severity_question <- dat.moral.numbers %>%
    dplyr::filter(question == "severity") %>%
    dplyr::distinct(experimentID) %>%
    dplyr::pull(experimentID)


## ----calculate-severity-differences--------------------------------------------------------------

# Severity question collapsed across scenarios. High values indicate higher horrificness ratings for the victims

dat.moral.numbers.severity.m.by.subj <- dat.moral.numbers %>%
    filter_vivacity_exps(excluded.exps = NULL, match.type = "exact") %>%
    dplyr::filter(stringr::str_detect(question, "severity")) %>%
    dplyr::group_by(experimentID, first.scenario.type, ResponseId, vivacity) %>%
    dplyr::summarize(
        N = dplyr::n(),
        p.victims.greater.saved = mean(rating.raw > 3.5),
        rating = mean(rating.raw),
        .groups = "drop"
    )

# Severity question collapsed across participants. High values indicate higher horrificness ratings for the victims
dat.moral.numbers.severity.m.by.scenarios <- dat.moral.numbers %>%
    dplyr::filter(experimentID %in% l_exp_with_single_severity_question) %>%
    dplyr::filter(stringr::str_detect(question, "severity")) %>%
    dplyr::group_by(experimentID, scenarioID, vivacity) %>%
    dplyr::summarize(
        N = dplyr::n(),
        p.victims.greater.saved = mean(rating.raw > 3.5),
        rating = mean(rating.raw),
        .groups = "drop"
    )


## ----find-bad-subj-severity-manipulation---------------------------------------------------------
# Identify participants who don't consider the deaths of the victims as more severe than that
# of the beneficiaries.
# We will not remove them from the data frame but rather create a new "experiment" (exp8.vivacityManipulationWorking) where only those participants are included for whom the vivacity manipulation worked

dat.moral.numbers.bad.subj.severity <- dplyr::bind_rows(
    # Find participants who, in the VIVID condition, do not consider the deaths of the victims as more severe
    # than that of the beneficiaries. This is assessed through a one-tailed binomial test, given the number of trials.
    # Just exclude participants based on 50% threshold to avoid an excessive rejection rate. See text below for a justification
    dat.moral.numbers.severity.m.by.subj %>%
        dplyr::filter(vivacity == "vivid") %>%
        # dplyr::filter(p.victims.greater.saved < get.min.number.of.correct.trials.by.binom.test(N, "greater")/N),
        dplyr::filter(p.victims.greater.saved <= .5),
    
    # Find participants who, in the NEUTRAL condition, DO consider the deaths of the victims as more severe
    # than that of the beneficiaries (or vice versa). This is assessed through a one-tailed binomial test,
    # given the number of trials.
    dat.moral.numbers.severity.m.by.subj %>%
        dplyr::filter(vivacity == "neutral") %>%
        dplyr::mutate(binom.threshold = purrr::map_dbl(N, ~ get.min.number.of.correct.trials.by.binom.test(.x, "greater") / .x)) %>%
        # dplyr::filter(p.victims.greater.saved <= (1 - get.min.number.of.correct.trials.by.binom.test(N, "greater")/N))
        dplyr::filter(
            (p.victims.greater.saved <= 1 - binom.threshold) |
                (p.victims.greater.saved >= binom.threshold)
        )
) %>%
    # dplyr::select(experimentID, ResponseId) %>%
    dplyr::select(experimentID, ResponseId, vivacity, p.victims.greater.saved) %>%
    dplyr::distinct()


## ----find-bad-subj-not-juddging-neutral-as-more-acceptable---------------------------------------
dat.moral.numbers.vivacity.bad.subj.acceptability <-
    dplyr::anti_join(
        dat.moral.numbers %>%
            filter_vivacity_exps() %>%
            dplyr::filter(question == "acceptability") %>%
            dplyr::group_by(experimentID, ResponseId, vivacity) %>%
            dplyr::summarise(
                rating.raw = mean(rating.raw),
                .groups = "drop"
            ),
        dat.moral.numbers.bad.subj.severity %>%
            dplyr::select(experimentID, ResponseId),
        by = c("experimentID", "ResponseId")
    ) %>%
    tidyr::pivot_wider(
        names_from = vivacity,
        values_from = rating.raw
    ) %>%
    dplyr::mutate(d_neutral_vivid = neutral - vivid) %>%
    dplyr::filter(d_neutral_vivid < 0)


## ----add-experiments-with-working-severity-manipulation------------------------------------------
# Now add new "experiments" using only those subjects for whom the vividness manipulation worked

dat.moral.numbers.vivacityManipulationWorking <- dplyr::anti_join(
    
    # Experiments with vivacity manipulation used as the base experiments....
    dat.moral.numbers %>%
        dplyr::filter(experimentID %in% l_exp_with_single_severity_question),
    
    # From which the combined bad subjects are subtracted      
    dplyr::bind_rows(
        dat.moral.numbers.bad.subj.severity,
        dat.moral.numbers.vivacity.bad.subj.acceptability
    ) %>%
        dplyr::distinct(experimentID, ResponseId),
    by = c("experimentID", "ResponseId")
) %>%
    # experimentID is updated
    dplyr::mutate(experimentID = stringr::str_c(experimentID, ".vivacityManipulationWorking"))


# Add these new "experiments"
dat.moral.numbers <- dplyr::bind_rows(
    dat.moral.numbers,
    dat.moral.numbers.vivacityManipulationWorking
)

# Delete temporary experiment
rm(dat.moral.numbers.vivacityManipulationWorking)


## ----calculate-averages-with-working-severity-manipulation---------------------------------------
# Add these averages across participants for the new "experiment"

dat.moral.numbers.m.across.subj <- dplyr::bind_rows(
    dat.moral.numbers.m.across.subj,
    
    dat.moral.numbers %>%
        dplyr::filter(
            stringr::str_detect(experimentID, "vivacityManipulationWorking"),
            question == "acceptability"
        ) %>%
        dplyr::select(-question) %>%
        {
            dplyr::bind_rows(
                # All blocks collapsed
                dplyr::mutate(., blocks = "all", .after = "experimentID"),
                
                # Just the first block
                dplyr::filter(., first.scenario.type == vivacity) %>%
                    dplyr::mutate(blocks = "first", .after = "experimentID")
            )
        } %>%
        dplyr::group_by(experimentID, blocks, decisionType, vivacity, ratio, n.victims, n.saved) %>%
        summarize.ratings(.groups = "drop") %>%
        dplyr::arrange(experimentID, blocks)
)


## ----create-list-of-experiment-with-and-without-working-vivacity-manipulation--------------------
# Like l_exp_with_single_severity_question, but also includes the .vivacityManipulationWorking
# variants added to dat.moral.numbers above. Uses match.type = "start" (the default) to
# capture both base experiment IDs and their .vivacityManipulationWorking counterparts.
l_exp_with_single_severity_question_augmented <- dat.moral.numbers %>%
    filter_vivacity_exps() %>%
    dplyr::distinct(experimentID) %>%
    dplyr::pull(experimentID)


## ----fit-augment-data-with-working-severity-manipulation-----------------------------------------
# Add "experiments" with vivacityManipulationWorking to existing data frames
# * dat.moral.numbers.m.across.subj.for.fit
# * dat.moral.numbers.for.bootstrap.fit

# For overall fit
dat.moral.numbers.m.across.subj.for.fit <- dplyr::bind_rows(
    dat.moral.numbers.m.across.subj.for.fit,
    dat.moral.numbers.m.across.subj %>%
        # Just in case
        dplyr::ungroup() %>%
        dplyr::filter(stringr::str_detect(experimentID, "vivacityManipulationWorking"))
)


# For bootstrap fit
dat.moral.numbers.for.bootstrap.fit <-
    dplyr::bind_rows(
        # Existing data
        dat.moral.numbers.for.bootstrap.fit,
        
        # New data with vivacityManipulationWorking
        dat.moral.numbers %>%
            dplyr::ungroup() %>% # Just in case
            dplyr::filter(
                stringr::str_detect(experimentID, "vivacityManipulationWorking"),
                question == "acceptability"
            ) %>%
            dplyr::select(-question) %>% 
            {
                dplyr::bind_rows(
                    # Collapsed across blocks
                    dplyr::mutate(., blocks = "all", .after = "experimentID"),
                    
                    # First block only
                    dplyr::filter(., first.scenario.type == vivacity) %>%
                        dplyr::mutate(blocks = "first", .after = "experimentID")
                )
            }
    ) %>%
    dplyr::arrange(experimentID, blocks)


## ----fit-overall-augment-with-working-severity-manipulation--------------------------------------
# Add "experiments" with vivacityManipulationWorking to existing data frames
# * dat.moral.numbers.overall.fit.1param
# * dat.moral.numbers.overall.fit.1param.linearized 
# * dat.moral.numbers.overall.fit.2param


dat.moral.numbers.overall.fit.1param <- dplyr::bind_rows(
    dat.moral.numbers.overall.fit.1param,
    dat.moral.numbers.m.across.subj.for.fit %>%
        dplyr::filter(stringr::str_detect(experimentID, "vivacityManipulationWorking")) %>%
        dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
        dplyr::group_modify(~ tryCatch(
            minpack.lm::nlsLM(
                rating.bin.M ~ acceptability.fnc(
                    w,
                    n.saved, n.victims
                ),
                data = .x,
                # This is a list
                start = FIT_PARAMS$start["w"],
                # nlsLM specific options
                # These are vectors
                lower = FIT_PARAMS$lower["w"],
                upper = FIT_PARAMS$upper["w"]
            ) %>%
                broom::tidy(),
            error = function(e) data.frame(w = NA)
        )) %>%
        # Just in case
        dplyr::ungroup()
)

dat.moral.numbers.overall.fit.1param.linearized <- dplyr::bind_rows(
    dat.moral.numbers.overall.fit.1param.linearized,
    dat.moral.numbers.m.across.subj.for.fit %>%
        dplyr::filter(stringr::str_detect(experimentID, "vivacityManipulationWorking")) %>%
        dplyr::rename(rating.bin = rating.bin.M) %>%
        dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
        dplyr::group_modify(~ tryCatch(
            get.weber.ratio.linearized(.x),
            error = function(e) data.frame(w = NA)
        )) %>%
        # Just in case
        dplyr::ungroup()
)

dat.moral.numbers.overall.fit.2param <- dplyr::bind_rows(
    dat.moral.numbers.overall.fit.2param,
    dat.moral.numbers.m.across.subj.for.fit %>%
        dplyr::filter(stringr::str_detect(experimentID, "vivacityManipulationWorking")) %>%
        dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
        dplyr::group_modify(~ tryCatch(
            minpack.lm::nlsLM(
                rating.bin.M ~ acceptability.fnc(w,
                                                 n.saved, n.victims,
                                                 a = a
                ),
                data = .x,
                # This is a list
                start = FIT_PARAMS$start,
                # nlsLM specific options
                # These are vectors
                lower = FIT_PARAMS$lower,
                upper = FIT_PARAMS$upper
            ) %>%
                broom::tidy(),
            error = function(e) data.frame(w = NA, a = NA)
        )) %>%
        # Just in case
        dplyr::ungroup()
)


## ----fit-bootstrap-calculate-with-working-severity-manipulation, include = FALSE-----------------
if (RECALCULATE_EVERYTHING) {
    # tic()
    gc() # Clean up memory just in
    warning("I'm about to generate ", N_BOOTSTRAP, " bootstrap samples. This will take about 8 min.")
    dat.moral.numbers.bootstrap.1param <- dplyr::bind_rows(
        dat.moral.numbers.bootstrap.1param,
        furrr::future_imap_dfr(1:(N_BOOTSTRAP / 1),
                               ~ dat.moral.numbers.for.bootstrap.fit %>%
                                   dplyr::filter(stringr::str_detect(experimentID, "vivacityManipulationWorking")) %>%
                                   dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
                                   dplyr::group_modify(~ bootstrap.weber.ratio(.x, fit.a = FALSE, fit.w = TRUE)),
                               .options = furrr_options(
                                   seed = TRUE,
                                   stdout = !RECALCULATE_EVERYTHING
                               )
        )
    )
    # toc()
    
    dat.moral.numbers.bootstrap.1param.summary <-
        dat.moral.numbers.bootstrap.1param %>%
        dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
        summarize_bootstrap_samples()
    #    # wilcox.test(w  ~ vivacity, data = .)
    
    warning("I'm about to generate ", N_BOOTSTRAP, " bootstrap samples. This will take about 8 min.")
    dat.moral.numbers.bootstrap.2param <- dplyr::bind_rows(
        dat.moral.numbers.bootstrap.2param,
        furrr::future_imap_dfr(1:(N_BOOTSTRAP / 1),
                               ~ dat.moral.numbers.for.bootstrap.fit %>%
                                   dplyr::filter(stringr::str_detect(experimentID, "vivacityManipulationWorking")) %>%
                                   dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
                                   dplyr::group_modify(~ bootstrap.weber.ratio(.x, fit.a = TRUE, fit.w = TRUE)),
                               .options = furrr_options(
                                   seed = TRUE,
                                   stdout = !RECALCULATE_EVERYTHING
                               )
        )
    )
    
    dat.moral.numbers.bootstrap.2param.summary <-
        dat.moral.numbers.bootstrap.2param %>%
        dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
        summarize_bootstrap_samples()
    #    # wilcox.test(w  ~ vivacity, data = .)
    
    # Save data
    save(
        dat.moral.numbers.bootstrap.1param,
        dat.moral.numbers.bootstrap.1param.summary,
        dat.moral.numbers.bootstrap.2param,
        dat.moral.numbers.bootstrap.2param.summary,
        file = here::here(
            OUTPUT_DIR, 
            "bootstrap.fits.RData"
        )
    )
    
    gc() # Clean up memory just in case
} else {
    # Just load the data
    # Already loaded
    # load(file.path(OUTPUT_DIR, "bootstrap.fits.RData"))
}


## ----exp-vivacity-plot-bin-filtered-unfiltered-prepare-------------------------------------------
l.exp.vivacity.model.comp.plots.bin <-
    l_exp_with_single_severity_question_augmented %>%
    purrr::set_names() %>%
    purrr::map(\(expID){
        c("all", "first") %>%
            purrr::set_names() %>%
            purrr::map(\(blcks){
                dat.moral.numbers %>%
                    dplyr::filter(experimentID == expID) %>%
                    # if blocks == "all" this will go through
                    dplyr::filter(blcks != "first" | first.scenario.type == vivacity) %>%
                    dplyr::filter(question == "acceptability") %>%
                    dplyr::mutate(vivacity = stringr::str_to_title(vivacity)) %>%
                    create.predictor.comparison.plot(
                        facet.var = NULL,
                        dat.fit = get_dat_fit_for_plot(
                            experimentID == expID,
                            blocks == blcks,
                            fit_type = FIT_FOR_FIGURE,
                            n_fit_params = 2
                        ),
                        value.var = rating.bin,
                        col.var = vivacity,
                        ylab = "Acceptability",
                        legend = "bottom",
                        add.fit = TRUE,
                        return.plot = FALSE,
                        add.p.saved.to.list = TRUE
                    )
            })
    })


## ----exp-vivacity-plot-raw-filtered-unfiltered-prepare-------------------------------------------

l.exp.vivacity.model.comp.plots.raw <-
    l_exp_with_single_severity_question_augmented %>%
    purrr::set_names() %>%
    purrr::map(\(expID){
        c("all", "first") %>%
            purrr::set_names() %>%
            purrr::map(\(blcks){
                dat.moral.numbers %>%
                    dplyr::filter(experimentID == expID) %>%
                    # if blocks == "all" this will go through
                    dplyr::filter(blcks != "first" | first.scenario.type == vivacity) %>%
                    dplyr::filter(question == "acceptability") %>%
                    create.predictor.comparison.plot(
                        facet.var = NULL,
                        value.var = rating.raw,
                        col.var = vivacity,
                        ylab = "Rating (raw)",
                        legend = "bottom",
                        add.fit = FALSE,
                        return.plot = FALSE,
                        add.p.saved.to.list = TRUE
                    )
            })
    })


## ----exp-vivacity-plot-define-plot-fnc, include = FALSE------------------------------------------
# Function moved to helper_functions/moral_numbers_plots.R


## ----exp8-11-plot-filtered-bin, fig.cap = "Binary acceptability ratings for Experiments 4, 5, and 6 as a function of the \\emph{beneficiary:victim} ratio, for both blocks combined. Filtered panels show only participants for whom the vivacity manipulation was successful (see main text for exclusion criteria); unfiltered panels show all participants. Error bars represent 95\\% bootstrap confidence intervals. Model fits are based on the average response of all participants.", fig.height = 12, fig.width = 10, out.height = "0.7\\textheight", out.extra = "keepaspectratio", include=FALSE----

l.exp.vivacity.model.comp.plots.bin %>% 
    purrr::imap(~ {
        
        expID <- .y %>% 
            stringr::str_remove("\\.vivacityManipulationWorking") 
        
        expID_paper <- format_exp_info(expID)
        
        isFiltered <- ifelse(
            stringr::str_detect(.y, "vivacityManipulationWorking"),
            " (filtered)",
            " (unfiltered)"
        )
        
        purrr::pluck(.x, "all", "plot.by.ratio") +
            ggplot2::theme(
                axis.text.x = ggplot2::element_text(angle = 45, vjust = 1, hjust = 1),
                axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 12)),
                legend.position = "bottom",
                plot.title = ggplot2::element_text(size = 10)
            ) +
            ggplot2::labs(title = stringr::str_wrap(stringr::str_c(expID_paper, isFiltered), 25)) +
            ggplot2::scale_x_continuous(
                breaks =
                    purrr::pluck(
                        l.moral.numbers.ratios,
                        expID
                    ),
                trans = "log2"
            )
        
    }
    ) %>% 
    ggpubr::ggarrange(
        plotlist = .,
        ncol = 3, nrow = 2,
        labels = "auto",
        legend = "bottom",
        common.legend = TRUE
    )




## ----exp8-plot-filtered, fig.cap = "Results of Experiment 4, where participants were filtered based on their responses on the severity question. Error bars represent 95\\% bootstrap confidence intervals. Model fits are based on the average response of all participants. (a) Binarized - both blocks, (b) Binarized - first block, (c) Raw - both blocks, (d) Raw - first block", fig.height=12, fig.width=10, out.height="0.7\\textheight", out.extra="keepaspectratio", include=FALSE----

combine_vivacity_detailed_plots("exp8.vivacityManipulationWorking")



## ----exp8-plot-unfiltered, fig.cap = "Results of Experiment 4 from all participants. Error bars represent 95\\% bootstrap confidence intervals. Model fits are based on the average response of all participants. (a) Binarized - both blocks, (b) Binarized - first block, (c) Raw - both blocks, (d) Raw - first block", fig.height=12, fig.width=10, out.height="0.7\\textheight", out.extra="keepaspectratio", include=FALSE----

combine_vivacity_detailed_plots("exp8")



## ----set-asymptotes------------------------------------------------------------------------------
# Also used for bootstrap samples, so define it here

v.asymptote.start <- c(
    "exp8" = 600,
    "exp10" = 500,
    "exp11" = 500
)


## ----relative-risk-calculate-boostrap-prepare-samples--------------------------------------------
if (RECALCULATE_EVERYTHING) {
    message("Preparing RR bootstrap samples - start")
    
    gc() # Clean up memory just in case
    
    dat.moral.numbers.bootstrap.samples <-
        l_exp_with_single_severity_question_augmented %>%
        purrr::set_names() %>%
        # Create the boostrap samples separate for each experiment, but rbind them below
        # This means that we have several repeats of the same entry in the boostrap_sample column, o
        # one for each experiment
        purrr::map(
            ~ rsample::group_bootstraps(
                data = dat.moral.numbers %>%
                    dplyr::filter(
                        experimentID == .x,
                        question == "acceptability"
                    ) %>%
                    dplyr::mutate(
                        rating.bin = factor(rating.bin),
                        asymptote.start = unname(v.asymptote.start[stringr::str_remove(
                            experimentID,
                            ".vivacityManipulationWorking"
                        )]),
                        ratio.range = factor(
                            ifelse(ratio > asymptote.start,
                                   "asymptote", "pre-asymptote"
                            ),
                            levels = c("pre-asymptote", "asymptote")
                        )
                    ),
                group = ResponseId,
                times = N_BOOTSTRAP
            )
        ) %>%
        purrr::list_rbind() %>% # names_to = "experimentID_bs") %>%
        dplyr::rename(bootstrap_sample = id)
    
    # Immediately save file
    saveRDS(
        dat.moral.numbers.bootstrap.samples,
        here::here(
            OUTPUT_DIR, 
            "dat.moral.numbers.bootstrap.samples.rds"
        )
    )
    
    message("Preparing RR bootstrap samples - end")
}


## ----relative-risk-calculate-boostrap-create-contingency-tables----------------------------------
# For each group defined by experimentID and ratio:
# Create a contingency table of vivacity (rows) by rating.bin (columns),
# where 0 and 1 in rating.bin represent binary outcomes


if (RECALCULATE_EVERYTHING) {
    gc()
    
    message("Preparing RR contingency tables - start")
    
    # Don't parallelize this as furrr 
    # The output is a data frame spanned by experimentID, ratio, and bootstrap_sample,
    # given the average (across subjects) for the acceptance rate in the vivid and the neutral condition 
    # and their ratio
    dat.moral.numbers.rr.by.ratios.bootstrap <-
        dat.moral.numbers.bootstrap.samples %>%
        dplyr::reframe(
            # Could use grouping instead of map2, but I'm a afraid of the memory overhead
            purrr::map2(
                # We have several repeats of the same entry in the boostrap_sample column, 
                # one for each experiment. This works as we group by experiment anyhow
                bootstrap_sample, splits,
                \(bs_id, bs_dat){
                    # Retrieve the data frame curresponding to the current split
                    rsample::analysis(bs_dat) %>%
                        dplyr::group_by(experimentID, ratio) %>%
                        dplyr::group_modify(
                            ~ .x %>%
                                create_acceptability_contingency_table() %>%
                                make_rr_neutral_vivid()
                            # ~ .x %>%
                            #     # For each group defined by experimentID and ratio:
                            #     # Create a contingency table of vivacity (rows) by rating.bin (columns),
                            #     # where 0 and 1 in rating.bin represent binary outcomes
                            #     janitor::tabyl(vivacity, rating.bin,
                            #                    show_missing_levels = TRUE
                            #     ) %>%
                            #     # Convert counts to row-wise proportions (i.e., percentage within each vivacity level)
                            #     janitor::adorn_percentages(denominator = "row") %>%
                            #     # drop `0` column as it's redundant
                            #     dplyr::select(any_of(c("vivacity", "1"))) %>%
                            #     tidyr::pivot_wider(
                            #         id_cols = NULL,
                            #         names_from = vivacity,
                            #         values_from = `1`
                            #     ) %>%
                            #     make_rr_neutral_vivid()
                        ) %>%
                        dplyr::ungroup() %>%
                        dplyr::mutate(bootstrap_sample = bs_id)
                }
            ) %>%
                purrr::list_rbind()
        )
    
    # Immediately save file
    saveRDS(
        dat.moral.numbers.rr.by.ratios.bootstrap,
        here::here(
            OUTPUT_DIR,
            "dat.moral.numbers.rr.by.ratios.bootstrap.rds"
        )
    )
    
    
    message("Preparing RR contingency tables - end")
}


## ----relative-risk-calculate-boostrap-regressions------------------------------------------------
if (RECALCULATE_EVERYTHING) {
    gc()
    
    
    message("Preparing RR regressions - start")
    
    
    # The input is a data frame spanned by experimentID, ratio, and bootstrap_sample,
    # given the average (across subjects) for the acceptance rate in the vivid and the neutral condition 
    # and their ratio
    dat.moral.numbers.rr.by.ratios.bootstrap.fit <-
        dat.moral.numbers.rr.by.ratios.bootstrap %>%
        dplyr::group_by(experimentID, bootstrap_sample) %>%
        dplyr::reframe(
            # Fit lm
            lm
            (
                RR ~ ratio,
                data = dplyr::pick(dplyr::everything())
            ) %>%
                broom::tidy() %>%
                dplyr::filter(term == "ratio") %>%
                dplyr::mutate(
                    cor = cor(RR, ratio, method = "spearman", use = "pairwise.complete.obs")
                )
        ) %>%
        dplyr::select(experimentID, bootstrap_sample, estimate, cor) %>%
        dplyr::rename(slope = estimate)
    
    
    dat.moral.numbers.rr.by.ratios.bootstrap.fit.summary <-
        dat.moral.numbers.rr.by.ratios.bootstrap.fit %>%
        dplyr::select(-bootstrap_sample) %>%
        dplyr::group_by(experimentID) %>%
        summarize_bootstrap_samples() %>%
        dplyr::mutate(
            slope_Z = slope_M / slope_SD,
            cor_Z = cor_M / cor_SD,
            dplyr::across(
                dplyr::ends_with("_Z"),
                list(p = ~ pnorm(.x, lower.tail = FALSE) * 2)
            )
        )
    
    # Immediately save files
    saveRDS(
        dat.moral.numbers.rr.by.ratios.bootstrap.fit,
        here::here(
            OUTPUT_DIR,
            "dat.moral.numbers.rr.by.ratios.bootstrap.fit.rds"
        )
    )
    
    saveRDS(
        dat.moral.numbers.rr.by.ratios.bootstrap.fit.summary,
        here::here(
            OUTPUT_DIR,"dat.moral.numbers.rr.by.ratios.bootstrap.fit.summary.rds"
        )
    )
    
    message("Preparing RR regressions - end")
}


## ----relative-risk-calculate-boostrap-save-data--------------------------------------------------
# if (RECALCULATE_EVERYTHING) {
#     # Objects have been saved above; a single RData file would almost 3GB and crash R
#     
#     # save(
#     #     dat.moral.numbers.bootstrap.samples,
#     #     dat.moral.numbers.rr.by.ratios.bootstrap,
#     #     dat.moral.numbers.rr.by.ratios.bootstrap.fit,
#     #     dat.moral.numbers.rr.by.ratios.bootstrap.fit.summary,
#     #     #dat.moral.numbers.rr.by.ratio.range.bootstrap,
#     #     file = file.path(OUTPUT_DIR, "bootstrap.rr.RData"))
#     
#     rm(dat.moral.numbers.bootstrap.samples, dat.moral.numbers.rr.by.ratios.bootstrap)
#     
#     gc() # Clean up memory just in case
#     
#     message("RR bootstrap - end")
# } else {
if (isFALSE(RECALCULATE_EVERYTHING)) {
    # Just load the data
    
    # R cannot load this 2.8G file :)
    # load(file.path(OUTPUT_DIR, "bootstrap.rr.RData"))
    
    # Just load the files we actually need.
    dat.moral.numbers.rr.by.ratios.bootstrap.fit.summary <-
        readRDS(here::here(
            OUTPUT_DIR,
            "dat.moral.numbers.rr.by.ratios.bootstrap.fit.summary.rds"
        ))
    
    dat.moral.numbers.rr.by.ratios.bootstrap.fit <-
        readRDS(here::here(
            OUTPUT_DIR,
            "dat.moral.numbers.rr.by.ratios.bootstrap.fit.rds"
        ))
    
    dat.moral.numbers.rr.by.ratios.bootstrap <-
        readRDS(here::here(
            OUTPUT_DIR,
            "dat.moral.numbers.rr.by.ratios.bootstrap.rds"
        ))
    
    
    # Heavy objects, don't load
    
    # dat.moral.numbers.bootstrap.samples <-
    #     readRDS(file.path(OUTPUT_DIR, "dat.moral.numbers.bootstrap.samples.rds"))
    #
}


# Should have saved this as well
dat.moral.numbers.rr.by.ratios.bootstrap.summary <- dat.moral.numbers.rr.by.ratios.bootstrap %>%
    dplyr::select(-bootstrap_sample) %>%
    dplyr::group_by(experimentID, ratio) %>%
    summarize_bootstrap_samples()


## ----create-rr-correlations-no-bootstrap, include = FALSE----------------------------------------

# Moved to helper_functions/moral_numbers_relative_risk.R
# create_acceptability_contingency_table <- function(dat, rating_col = rating.bin, cond_col = vivacity){
#
#     rating_col <- rlang::enquo(rating_col)
#     cond_col <- rlang::enquo(cond_col)
#     cond_col_name <- rlang::as_label(cond_col)
#
#     dat %>%
#         # make sure it's a factor
#         dplyr::mutate(
#             !!rating_col := as.factor(!!rating_col)
#         ) %>%
#         # Create a contingency table of vivacity (rows) by rating.bin (columns),
#         # where 0 and 1 in rating.bin represent binary outcomes
#         janitor::tabyl(
#             !!cond_col,
#             !!rating_col,
#             show_missing_levels = TRUE
#         ) %>%
#         # Convert counts to row-wise proportions (i.e., percentage within each vivacity level)
#         janitor::adorn_percentages(
#             denominator = "row"
#         ) %>%
#         # drop `0` column as it's redundant
#         dplyr::select(
#             any_of(c(cond_col_name, "1"))
#         ) %>%
#         tidyr::pivot_wider(
#             id_cols = NULL,
#             names_from = !!cond_col,
#             values_from = `1`
#         )
# }


dat.moral.numbers %>%
    dplyr::filter(
        experimentID %in% l_exp_with_single_severity_question_augmented,
        question == "acceptability"
    ) %>%
    dplyr::group_by(experimentID, ratio) %>% 
    dplyr::group_modify(
        \(df, key) {
            df %>% 
                create_acceptability_contingency_table() %>% 
                make_rr_neutral_vivid()
        }
    ) %>% 
    dplyr::group_by(experimentID) %>%
    rstatix::cor_test(
        RR,
        ratio,
        method = "spearman",
        use = "pairwise.complete.obs"
    ) %>% 
    dplyr::select(-dplyr::starts_with("var"), -method, -statistic) %>%
    format_exp_info() %>%
    dplyr::arrange(experimentID) %>%
    fix_zero_p_values(p) %>%
    knitr::kable(
        caption = "Spearman correlations between the relative risk ($\\RR = P_{\\text{acceptable, vivid}} / P_{\\text{acceptable, neutral}}$) and the \\textit{beneficiary:victim} ratio in Experiments 4 to 6, computed from observed (non-bootstrapped) acceptance rates. A positive $\\rho$ indicates that the difference in acceptability between the vivid and neutral conditions diminishes as the ratio increases.",
        col.names = c("Experiment", "$\\rho$", "$p$"),
        booktabs = TRUE,
        escape = FALSE) %>%
    kableExtra::kable_classic_2()    







## ----relative-risk-plot-prepare-separate-plots---------------------------------------------------

l_moral_numbers_rr_plot_filtered_unfiltered <-
    setdiff(l_exp_with_single_severity_question, "exp9a") %>%
    purrr::set_names() %>%
    purrr::map(~ create_rr_plot(.x, TRUE)) %>%
    # Just keep the correlation plot since we didn't manage to identify the aysmptotic range.
    purrr::map(
        ~ purrr::pluck(.x, "by_ratios")
    )


l_moral_numbers_rr_plot_filtered <-
    dat.moral.numbers %>%
    dplyr::distinct(experimentID) %>%
    filter_vivacity_exps() %>%
    dplyr::filter(stringr::str_detect(experimentID, "vivacityManipulationWorking")) %>%
    tibble::deframe() %>%
    purrr::set_names() %>%
    purrr::map(~ create_rr_plot(.x, FALSE)) %>%
    # Just keep the correlation plot since we didn't manage to identify the aysmptotic range.
    purrr::map(
        ~ purrr::pluck(.x, "by_ratios")
    )

l_moral_numbers_rr_plot_unfiltered <-
    dat.moral.numbers %>%
    dplyr::distinct(experimentID) %>%
    filter_vivacity_exps() %>%
    dplyr::filter(!stringr::str_detect(experimentID, "vivacityManipulationWorking")) %>%
    tibble::deframe() %>%
    purrr::set_names() %>%
    purrr::map(~ create_rr_plot(.x, FALSE)) %>%
    # Just keep the correlation plot since we didn't manage to identify the aysmptotic range.
    purrr::map(
        ~ purrr::pluck(.x, "by_ratios")
    )


## ----relative-risk-plot-prepare-facets, eval = FALSE---------------------------------------------
# 
# l_moral_numbers_rr_plot_filtered_facets <- create_rr_plot(
#     stringr::str_c(
#         setdiff(l_exp_with_single_severity_question, "exp9a"),
#         ".vivacityManipulationWorking"
#     ),
#     TRUE
# )
# 
# 
# .x, FALSE)) %>%
#     # Just keep the correlation plot since we didn't manage to identify the aysmptotic range.
#     purrr::map(
#         ~ purrr::pluck(.x, "by_ratios")
#     )
# 
# l_moral_numbers_rr_plot_unfiltered_facets <-
#     dat.moral.numbers %>%
#     dplyr::distinct(experimentID) %>%
#     filter_vivacity_exps() %>%
#     dplyr::filter(!stringr::str_detect(experimentID, "vivacityManipulationWorking")) %>%
#     tibble::deframe() %>%
#     purrr::set_names() %>%
#     purrr::map(~ create_rr_plot(.x, FALSE)) %>%
#     # Just keep the correlation plot since we didn't manage to identify the aysmptotic range.
#     purrr::map(
#         ~ purrr::pluck(.x, "by_ratios")
#     )


## ----exp8-relative-risk-plot, fig.cap = "Relative risk of acceptability as a function of ratio in Experiment 4.", fig.height=9, eval = SHOW_INDIVIDUAL_PLOTS----
# l_moral_numbers_rr_plot_filtered_unfiltered$exp8


## ----fits-bootstrap-with-forced-w-calculate------------------------------------------------------
dat.w.chosen.neutral <- dat.moral.numbers.bootstrap.1param.summary %>%
    dplyr::ungroup() %>%
    filter_vivacity_exps() %>%
    dplyr::filter(
        blocks %in% c("all", "first"),
        vivacity == "neutral"
    ) %>%
    dplyr::select(experimentID, blocks, w_M)


if (RECALCULATE_EVERYTHING) {
    gc() # Clean up memory just in case
    
    dat.moral.numbers.bootstrap.forced.w <- furrr::future_imap_dfr(
        1:(N_BOOTSTRAP / 1),
        ~ dat.moral.numbers.for.bootstrap.fit %>%
            dplyr::ungroup() %>%
            filter_vivacity_exps() %>%
            dplyr::filter(
                blocks %in% c("all", "first")
            ) %>%
            dplyr::group_by(experimentID, blocks, vivacity) %>%
            dplyr::group_modify(
                ~ bootstrap.weber.ratio(.x,
                                        fit.a = TRUE, fit.w = FALSE,
                                        w.chosen = dat.w.chosen.neutral %>%
                                            dplyr::filter(
                                                experimentID == unique(.x$experimentID),
                                                blocks == unique(.x$blocks)
                                            ) %>%
                                            dplyr::pull(w_M)
                ),
                .keep = TRUE
            ),
        .options = furrr_options(
            seed = TRUE,
            stdout = !RECALCULATE_EVERYTHING
        )
    ) %>%
        dplyr::ungroup()
    
    save(
        dat.moral.numbers.bootstrap.forced.w,
        file = here::here(
            OUTPUT_DIR,
            "bootstrap.fits.forced.w.RData"
        )
    )
    
    gc() # Clean up memory just in case
} else {
    # Just load the data
    load(here::here(
        OUTPUT_DIR,
        "bootstrap.fits.forced.w.RData"
    )
    )
}


## ----exp8-fits-bootstrap-with-forced-w-plot, fig.cap = "Bootstrap samples of the $\\alpha$ parameter in Experiment 4 (was 8), with the $w$ parameter set to the bootstrap fit obtained in the neutral condition.", eval = SHOW_INDIVIDUAL_PLOTS----
# dat.moral.numbers.bootstrap.forced.w %>%
#     dplyr::filter(stringr::str_starts(experimentID, "exp8")) %>%
#     format_exp_info() %>%
#     na.omit() %>%
#     ggplot2::ggplot(ggplot2::aes(x = vivacity, y = a)) %>%
#     violin_plot_template(yintercept = 1, add.dot.plot = FALSE) +
#     ggplot2::labs(y = latex2exp::TeX("$\\alpha$")) +
#     # ggplot2::scale_y_log10()
#     ggplot2::facet_wrap(blocks ~ experimentID, scales = "free", labeller = labeller(blocks = label_both))


## ----exp10-plot-filtered, fig.cap = "Results of Experiment 5, where participants were filtered based on their responses on the severity question. Error bars represent 95\\% bootstrap confidence intervals. Model fits are based on the average response of all participants. (a) Binarized - both blocks, (b) Binarized - first block, (c) Raw - both blocks, (d) Raw - first block", fig.height=12, fig.width=10, out.height="0.7\\textheight", out.extra="keepaspectratio", include=FALSE----

combine_vivacity_detailed_plots("exp10.vivacityManipulationWorking")



## ----exp10-plot-bin-unfiltered-block12, fig.cap = "Results of Experiment 5 from all participants. Error bars represent 95\\% bootstrap confidence intervals. Model fits are based on the average response of all participants. (a) Binarized - both blocks, (b) Binarized - first block, (c) Raw - both blocks, (d) Raw - first block", fig.height=12, fig.width=10, out.height="0.7\\textheight", out.extra="keepaspectratio", include=FALSE----

combine_vivacity_detailed_plots("exp10")


## ----exp10-relative-risk-plot, fig.cap = "Relative risk of acceptability as a function of ratio in Experiment 5.", fig.height=9, eval = SHOW_INDIVIDUAL_PLOTS----
# l_moral_numbers_rr_plot_filtered_unfiltered$exp10


## ----save-stuff-for-llms, eval = FALSE-----------------------------------------------------------
# save(
#     dat.moral.numbers,
#     dat.moral.numbers.exp2.m,
#     dat.moral.numbers.m.across.subj.for.fit,
#     dat.moral.numbers.overall.fit.1param,
#     dat.moral.numbers.overall.fit.2param,
#     file = here::here(
#         OUTPUT_DIR,
#         "tmp_stuff_for_llm.RData"
#     )
# )


## ----exp10-fits-bootstrap-with-forced-w-plot, fig.cap = "Bootstrap samples of the $\\alpha$ parameter in Experiment 5 (was 10), with the $w$ parameter set to the bootstrap fit obtained in the neutral condition.", eval = SHOW_INDIVIDUAL_PLOTS----
# dat.moral.numbers.bootstrap.forced.w %>%
#     dplyr::filter(
#         stringr::str_detect(experimentID, "exp10")) %>%
#     na.omit() %>%
#     format_exp_info() %>%
#     ggplot2::ggplot(ggplot2::aes(x = vivacity, y = a)) %>%
#     violin_plot_template(yintercept = 1, add.dot.plot = FALSE) +
#     ggplot2::labs(y = latex2exp::TeX("$\\alpha$")) +
#     # ggplot2::scale_y_log10()
#     ggplot2::facet_wrap(blocks ~ experimentID, scales = "free", labeller = labeller(blocks = label_both))


## ----exp11-plot-filtered, fig.cap = "Results of Experiment 6, where participants were filtered based on their responses on the severity question. Error bars represent 95\\% bootstrap confidence intervals. Model fits are based on the average response of all participants. (a) Binarized - both blocks, (b) Binarized - first block, (c) Raw - both blocks, (d) Raw - first block", fig.height=12, fig.width=10, out.height="0.7\\textheight", out.extra="keepaspectratio", include=FALSE----

combine_vivacity_detailed_plots("exp11.vivacityManipulationWorking")



## ----exp11-plot-unfiltered, fig.cap = "Results of Experiment 6, where all participants were included. Error bars represent 95\\% bootstrap confidence intervals. Model fits are based on the average response of all participants. (a) Binarized - both blocks, (b) Binarized - first block, (c) Raw - both blocks, (d) Raw - first block", fig.height=12, fig.width=10, out.height="0.7\\textheight", out.extra="keepaspectratio", include=FALSE----

combine_vivacity_detailed_plots("exp11")


## ----exp11-relative-risk-plot, fig.cap = "Relative risk of acceptability as a function of ratio in Experiment 6.", fig.height=9, eval=SHOW_INDIVIDUAL_PLOTS----
# l_moral_numbers_rr_plot_filtered_unfiltered$exp11


## ----relative-risk-plot, fig.cap = "Relative risk of acceptability ($\\RR = P_{\\text{acceptable, vivid}} / P_{\\text{acceptable, neutral}}$) as a function of the \\textit{beneficiary:victim} ratio in Experiments 4 to 6, restricted to participants for whom the vividness manipulation was effective. Points and error bars represent bootstrap means and 95\\% percentile intervals. The line shows a linear fit on the log2-transformed ratio scale.", fig.height=9, eval = FALSE----
# l_moral_numbers_rr_plot_filtered %>%
#     purrr::imap(~ .x + ggplot2::ggtitle(format_exp_info(.y))) %>%
#     ggpubr::ggarrange(
#         plotlist = .,
#         ncol = 1,
#         labels = "auto",
#         common.legend = FALSE,
#         legend = "bottom"
#     )
# 


## ----relative-risk-plot-unfiltered, fig.cap = "Relative risk of acceptability as a function of the \\textit{beneficiary:victim} ratio in Experiments 4 to 6, including all participants regardless of their responses on the severity question. Points and error bars represent bootstrap means and 95\\% percentile intervals. The line shows a linear fit on the log2-transformed ratio scale. Compare with Figure~\\ref{fig:relative-risk-plot} for results restricted to participants for whom the vividness manipulation was effective.", fig.height=9, eval = FALSE----
# l_moral_numbers_rr_plot_unfiltered %>%
#     purrr::imap(~ .x + ggplot2::ggtitle(format_exp_info(.y))) %>%
#     ggpubr::ggarrange(
#         plotlist = .,
#         ncol = 1,
#         labels = "auto",
#         common.legend = FALSE,
#         legend = "bottom"
#     )
# 


## ----relative-risk-slope-print, include=FALSE----------------------------------------------------
dat.moral.numbers.rr.by.ratios.bootstrap.fit.summary %>%
    dplyr::select(experimentID, dplyr::starts_with("slope"), dplyr::starts_with("cor")) %>%
    format_exp_info() %>%
    dplyr::arrange(experimentID) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("_Z_p"), fix_zero_p_values)) %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.numeric), scales::label_scientific(digits = 3))) %>%
    dplyr::select(-dplyr::ends_with("_N")) %>%
    knitr::kable(
        caption = "Bootstrap estimates of the linear regression slope and Spearman correlation between the relative risk ($\\RR = P_{\\text{acceptable, vivid}} / P_{\\text{acceptable, neutral}}$) and the \\textit{beneficiary:victim} ratio in Experiments 4 to 6 (10,000 bootstrap samples). Each bootstrap sample yields average acceptance rates per ratio and condition; we then derive $\\RR$ and regress it against the \\textit{beneficiary:victim} ratio. The table reports the mean estimate, its Monte Carlo (MC) error, the $z$-score, the two-tailed $p$-value, and the 95\\% percentile interval (PI) for both the regression slope and the Spearman correlation.",
        col.names = c(
            "Experiment",
            names(.)[-1] %>%
                stringr::str_remove("^(slope_|cor_)") %>%
                stringr::str_remove("^pi.") %>%
                stringr::str_replace("Z_p", "p") %>%
                stringr::str_replace("mc_error", "MC Error")
        ),
        booktabs = TRUE,
        longtable = FALSE,
        escape = FALSE
    ) %>%
    kableExtra::add_header_above(c(" " = 4, "95% PI" = 2, " " = 5, "95% PI" = 2, " " = 2), bold = FALSE) %>%
    kableExtra::add_header_above(c(" " = 1, "Regression slope" = 7, "Spearman correlation" = 7), bold = TRUE) %>%
    kableExtra::kable_styling(latex_options = c(
        "striped",
        "scale_down",
        "hold_position"
    )) %>%
    kableExtra::kable_classic_2()


## ----bootstrap-fits-print, include=FALSE---------------------------------------------------------

dat_bootstrap_fit_1_2_param_for_table <- 
    list(
        `One-parameter fits` = dat.moral.numbers.bootstrap.1param.summary %>%
            # dplyr::filter(stringr::str_detect(experimentID, "^exp(1|1b|4|8|10|11)$")),
            dplyr::filter(stringr::str_detect(experimentID, "^exp(1|1b|4)$")) %>%
            add_z_relative_to_baseline(decisionType),
        `Two-parameter fits` = dat.moral.numbers.bootstrap.2param.summary %>%
            filter_vivacity_exps() %>%
            add_z_relative_to_baseline(vivacity)
    ) %>%
    purrr::map(
        ~ {
            .x %>%
                dplyr::mutate(cond = dplyr::coalesce(decisionType, vivacity)) %>%
                dplyr::select(
                    experimentID, blocks, cond,
                    dplyr::ends_with("_N"),
                    dplyr::ends_with("_M"),
                    dplyr::ends_with("_mc_error"),
                    dplyr::matches("_pi\\."),
                    dplyr::matches("_Z(_p)*$")
                )
        }
    ) %>%
    purrr::list_rbind(names_to = "type") %>%
    format_exp_info() %>%
    dplyr::arrange(type, experimentID, blocks, cond) %>%
    dplyr::filter(blocks == "all") %>%
    dplyr::select(-blocks, -a_N)

dat_bootstrap_fit_1_2_param_for_table %>%
    dplyr::select(-type) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("_Z_p"), fix_zero_p_values)) %>%
    knitr::kable(
        caption = "One and two parameter bootstrap fits with bootstrap estimates and 95\\% percentile intervals. Fits were obtained for from all available blocks.",
        col.names = c(
            "Experiment",
            # "Blocks",
            "Condition",
            "N",
            rep(c(" ", " ", "lower", "upper", " ", " "), 2)
        ),
        booktabs = TRUE,
        longtable = FALSE,
        escape = FALSE
    ) %>%
    kableExtra::kable_styling(latex_options = c(
        "striped",
        "scale_down",
        "hold_position",
        "repeat_header"
    )) %>%
    kableExtra::add_header_above(c(" " = 3, "M" = 1, "MC Error" = 1, "95% PI" = 2, "Z" = 1, "p" = 1, "M" = 1, "MC Error" = 1, "95% PI" = 2, "Z" = 1, "p" = 1), bold = FALSE) %>%
    kableExtra::add_header_above(c(" " = 3, "$w$" = 6, "$\\\\alpha$" = 6), bold = FALSE, align = "c", escape = FALSE) %>%
    kableExtra::kable_classic_2() %>%
    # For long table, pack_rows must come last
    # See https://github.com/haozhu233/kableExtra/issues/476
    kableExtra::pack_rows(index = dat_bootstrap_fit_1_2_param_for_table %>%
                              make.pack.index(type))


## ----fits-bootstrap-with-forced-w-print, include=FALSE-------------------------------------------
dat.moral.numbers.bootstrap.forced.w.for.table <- dplyr::left_join(
    dat.moral.numbers.bootstrap.forced.w %>%
        dplyr::group_by(experimentID, blocks, vivacity) %>%
        summarize_bootstrap_samples() %>%
        dplyr::mutate(
            a_Z_vs_1 = (a_M - 1) / a_SD,
            a_p_Z_vs_1 = pnorm(a_Z_vs_1, lower.tail = FALSE) * 2
        ),
    dat.w.chosen.neutral,
    by = c("experimentID", "blocks")
) %>%
    dplyr::relocate(w_M, .after = "vivacity") %>%
    dplyr::select(-dplyr::ends_with("_mc_error")) %>%
    tidyr::pivot_wider(
        id_cols = c(experimentID, blocks, w_M),
        names_from = vivacity,
        values_from = dplyr::starts_with("a_"),
        names_vary = "slowest"
    ) %>%
    # Add Z score vivid vs. neutral
    dplyr::mutate(
        a_Z_vs_neutral = (a_M_vivid - a_M_neutral) / a_SD_neutral,
        a_p_Z_vs_neutral = pnorm(a_Z_vs_neutral, lower.tail = FALSE) * 2
    ) %>%
    dplyr::rename("w (neutral)" = w_M) %>%
    dplyr::filter(blocks == "all") %>%
    dplyr::select(-blocks)

dat.moral.numbers.bootstrap.forced.w.for.table %>% 
    format_exp_info() %>%
    dplyr::mutate(
        dplyr::across(
            dplyr::matches("p_Z"),
            fix_zero_p_values
        )
    ) %>% 
    dplyr::arrange(experimentID) %>%
    kable.packed(
        "experimentID",
        caption = "Bootstrap fits for the $\\alpha$ parameter, with the $w$ set to the one-parameter bootstrap fit from the neutral condition (Experiments 4 to 6). The data were obtained from all available blocks. For both the neutral and the vivid condition, the tables shows the number of bootstrap samples, the bootstrap mean, standard deviation, Z score against the neutral value of 1 and its associated p value as well as the upper and lower bound of the 95\\% bootstrap percentile interval. For the vivid condition, the table also shows the Z score of the estimate relative to the mean and standard deviation in the neutral condition.",
        col.names = names(dat.moral.numbers.bootstrap.forced.w.for.table[, -1]) %>%
            stringr::str_remove("^a_") %>%
            stringr::str_remove("_(neutral|vivid)$") %>%
            stringr::str_remove("^pi.") %>%
            stringr::str_remove("(_Z)*_vs(_.+)*$"),
        booktabs = TRUE,
        escape = FALSE,
        longtable = FALSE
    ) %>%
    kableExtra::add_header_above(
        c(
            " " = 4,
            "95% pecentile interval" = 2,
            "Z score vs. 1" = 2,
            " " = 3,
            "95% pecentile interval" = 2,
            "Z score vs. 1" = 2,
            # "Z score vs. neutral" = 2),
            " " = 2
        ),
        italic = TRUE,
        bold = FALSE
    ) %>%
    kableExtra::add_header_above(
        c(" " = 1, "Neutral" = 7, "Vivid" = 7, "Vivid vs. Neutral" = 2),
        bold = TRUE
    ) %>%
    # kableExtra::add_header_above(c(" " = 6, "95% Percentile interval" = 2)) %>%
    kableExtra::kable_styling(latex_options = c(
        "striped",
        "scale_down",
        "hold_position",
        "repeat_header"
    )) %>%
    kableExtra::kable_classic_2()


## ----exp11-fits-bootstrap-with-forced-w-plot, fig.cap = "Bootstrap samples of the $\\alpha$ parameter in Experiment 6 (was 11), with the $w$ parameter set to the bootstrap fit obtained in the neutral condition.", eval = SHOW_INDIVIDUAL_PLOTS----
# dat.moral.numbers.bootstrap.forced.w %>%
#     dplyr::filter(stringr::str_detect(experimentID, "exp11")) %>%
#     na.omit() %>%
#     format_exp_info() %>%
#     ggplot2::ggplot(ggplot2::aes(x = vivacity, y = a)) %>%
#     violin_plot_template(yintercept = 1, add.dot.plot = FALSE) +
#     ggplot2::labs(y = latex2exp::TeX("$\\alpha$")) +
#     # ggplot2::scale_y_log10()
#     ggplot2::facet_wrap(blocks ~ experimentID, scales = "free", labeller = labeller(blocks = label_both))


## ----fits-bootstrap-with-forced-w-plot-prepare---------------------------------------------------
# dat.moral.numbers.bootstrap.forced.w %>%
#     filter_vivacity_exps() %>%
#     dplyr::filter(blocks == "all") %>%
#     na.omit() %>%
#     format_exp_info() %>%
#     ggplot2::ggplot(ggplot2::aes(x = vivacity, y = a)) %>%
#     violin_plot_template(yintercept = 1, add.dot.plot = FALSE) +
#     ggplot2::labs(y = latex2exp::TeX("$\\alpha$")) +
#     # ggplot2::scale_y_log10()
#     ggplot2::facet_wrap(
#         . ~ experimentID,
#         scales = "free",
#         ncol = 2
#     )


l_plots_fits_bootstrap_a_with_forced_w <- l_exp_with_single_severity_question_augmented %>% 
    purrr::set_names() %>% 
    purrr::map(\(expID){
        dat.moral.numbers.bootstrap.forced.w %>% 
            dplyr::filter(experimentID == expID,
                          blocks == "all") %>% 
            na.omit() %>%
            dplyr::mutate(vivacity = stringr::str_to_title(vivacity)) %>% 
            ggplot2::ggplot(ggplot2::aes(x = vivacity, y = a)) %>%
            violin_plot_template(yintercept = 1, add.dot.plot = FALSE) +
            ggplot2::labs(y = latex2exp::TeX("$\\alpha$")) +
            ggplot2::ggtitle(format_exp_info(expID, merge_filtering = TRUE))
        
    }
    ) 



## ----rr-vs-weber-ratio-and-alpha-bootstrap-filtered-plot, fig.cap = "Relative risk of acceptability ($\\RR = P_{\\text{acceptable, vivid}} / P_{\\text{acceptable, neutral}}$, left panels) and bootstrap distribution of the attentional weight of victims parameter ($\\alpha$, right panels) in Experiments 4 to 6, restricted to participants for whom the vividness manipulation was effective. In the left panels, circles show bootstrap means and error bars show 95\\% percentile intervals (10,000 bootstrap samples); the line shows a linear regression fit and the shaded area its 95\\% confidence interval. The $w$ parameter is fixed to the bootstrap estimate from the neutral condition. The horizontal line marks $\\alpha = 1$ (no shift in attentional weight).", fig.width = 8, fig.height = 13, out.height = "0.7\\textheight", out.extra = "keepaspectratio"----

# Pair RR and alpha plots per experiment, stripping individual titles and borders
purrr::map2(
    l_moral_numbers_rr_plot_filtered,
    l_plots_fits_bootstrap_a_with_forced_w %>%
        purrr::keep_at(\(p) stringr::str_detect(p, "vivacityManipulationWorking")),
    \(rr, a) {
        list(
            rr,
            a
        ) %>%
            purrr::map(\(x) x +
                           ggplot2::labs(title = NULL) +
                           ggplot2::theme(plot.background = ggplot2::element_rect(colour = NA, fill = NA))
            )
    }
) %>%
    # Arrange each pair side by side, add experiment name as row title, and border the group
    # (map2 inherits names from first argument, used by imap as `name`)
    purrr::imap(\(pair, name) {
        p <- ggpubr::ggarrange(
            plotlist = pair,
            ncol = 2,
            nrow = 1,
            widths = c(2, 1)
        ) %>%
            ggpubr::annotate_figure(
                top = ggpubr::text_grob(format_exp_info(name), face = "bold")
            )
        cowplot::ggdraw(p) +
            ggplot2::theme(plot.background = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1))
    }) %>%
    # Stack rows for all experiments into a single figure
    ggpubr::ggarrange(
        plotlist = .,
        ncol = 1,
        nrow = 3,
        labels = "auto",
        common.legend = FALSE,
        legend = "bottom"
    )



## ----exp11-strategies, eval = FALSE--------------------------------------------------------------
# # passed attention check
# 
# dat.moral.numbers %>%
#     dplyr::filter(experimentID == "exp11") %>%
#     dplyr::distinct(ResponseId, strategies)


## ----exp11-verification-counter-balancing, eval = FALSE------------------------------------------
# dat.moral.numbers %>%
#     dplyr::filter(experimentID == "exp11") %>%
#     dplyr::distinct(first.scenario.type, number.counterbalancing.cond, ResponseId) %>%
#     dplyr::count(first.scenario.type, number.counterbalancing.cond)


## ----exp11-plot-hist-difference-vivacity, eval = FALSE-------------------------------------------
# dat.moral.numbers %>%
#     filter_vivacity_exps() %>%
#     dplyr::filter(
#         question == "acceptability",
#         ratio <= 100000000000
#     ) %>%
#     # Scenarios occur with different numbers in different blocks, so we can't pair them
#     dplyr::group_by(experimentID, ResponseId, vivacity) %>%
#     dplyr::summarise(
#         rating.raw = mean(rating.raw),
#         .groups = "drop"
#     ) %>%
#     tidyr::pivot_wider(
#         id_cols = c(experimentID, ResponseId),
#         names_from = vivacity,
#         values_from = rating.raw
#     ) %>%
#     dplyr::mutate(d_neutral_vivid = neutral - vivid) %>%
#     format_exp_info() %>%
#     ggplot2::ggplot(ggplot2::aes(x = d_neutral_vivid)) +
#     ggplot2::geom_histogram(ggplot2::aes(y = ..density..), color = "#0000AA", fill = "#0000AA") +
#     ggplot2::geom_density(alpha = .8, fill = "#6666FF") +
#     #    theme_black (base_size = 16)+
#     ggplot2::labs(x = "Difference between neutral and vivid ratings per participant") +
#     ggplot2::geom_vline(xintercept = 0) +
#     # ggplot2::xlim(0,3)
#     ggplot2::facet_wrap(~experimentID) # , scales = "free")
# 
# 
# # ggsave2("/Users/endress/Downloads/histogram.jpg")


## ----exp11-vivacity-verification, eval = FALSE---------------------------------------------------
# dat.moral.numbers %>%
#     filter_vivacity_exps() %>%
#     dplyr::filter(
#         question == "acceptability"
#     ) %>%
#     # Scenarios occur with different numbers in different blocks, so we can't pair them
#     dplyr::group_by(experimentID, ResponseId, scenarioID, vivacity) %>%
#     dplyr::summarise(
#         rating.raw = mean(rating.raw),
#         .groups = "drop"
#     ) %>%
#     dplyr::group_by(experimentID, ResponseId) %>%
#     dplyr::mutate(
#         v = var(rating.raw)
#     ) %>%
#     dplyr::filter(
#         # Exclude constant subjects
#         v > 0
#     ) %>%
#     rstatix::wilcox_test(rating.raw ~ vivacity,
#                          alternative = "less",
#                          detailed = TRUE,
#                          paired = FALSE
#     ) %>%
#     rstatix::add_significance() %>%
#     dplyr::filter(
#         # Having positive differences is expected
#         estimate < 0,
#         p.signif != "ns"
#     )


## ----llm-names-----------------------------------------------------------------------------------
LLM_LABELS_TIBBLE <- tibble::tribble(
    ~model_type, ~model, ~label, ~reasoning_effort,
    
    # Cloud models
    "cloud", "gpt-4o-mini", "GPT-4o mini", NA_character_,
    # "gpt-4o",
    "cloud", "gpt-4.1", "GPT-4.1", NA_character_,
    "cloud", "gpt-4.1-mini", "GPT-4.1 mini", NA_character_,
    "cloud", "gpt-5-mini", "GPT-5 mini", "minimal",
    "cloud", "gpt-5-mini", "GPT-5 mini", "medium",
    "cloud", "gpt-5-mini", "GPT-5 mini", "high",
    "cloud", "claude-sonnet-4-5-20250929", "Claude Sonnet 4.5", NA_character_,
    "cloud", "gemini-2.5-flash", "Gemini 2.5 flash", NA_character_,
    "cloud", "gemini-2.5-pro", "Gemini 2.5 pro", NA_character_,
    "cloud", "mistral-medium-latest", "Mistral Medium", NA_character_,
    "cloud", "deepseek-chat", "Deepseek", NA_character_,
    "cloud", "qwen-plus", "Qwen-plus", NA_character_,
    
    # Local models
    "local", "llama3.2", "Llama 3.2 (3.2B)", NA_character_,
    "local", "mistral", "Mistral (7.2B)", NA_character_,
    "local", "mistral-nemo", "Mistral nemo (12.2B)", NA_character_,
    NA_character_, "participants", "Humans", NA_character_
) %>%
    dplyr::mutate(
        model = dplyr::if_else(is.na(reasoning_effort),
                               model,
                               stringr::str_c(model, "-reasoning-", reasoning_effort)
        ),
        label = dplyr::if_else(is.na(reasoning_effort),
                               label,
                               stringr::str_c(label, " (", reasoning_effort, " reasoning)")
        )
    )



## ----plot-llm-exp2-joint-preference-scatter-preview, fig.cap="XXX update"------------------------

target_plot <- here::here(
    "code",
    "moral_numbers_files", 
    "figure-latex",
    "plot-llm-exp2-joint-preference-scatter-1.pdf"
)

if (file.exists(target_plot)) {
    # If the file exists from a previous render, display it as a static image
    knitr::include_graphics(target_plot)
} else {
    # If the file is missing, generate a safe ggplot placeholder
    ggplot2::ggplot() +
        ggplot2::annotate("label", x = 0.5, y = 0.5, 
                          label = paste0("PLACEHOLDER: SI PLOT NOT FOUND\n\n",
                                         "Expected: ", target_plot, "\n\n",
                                         "Please render the Appendix chunk first."),
                          size = 4, fill = "white", color = "red", label.size = 1) +
        ggplot2::theme_void() +
        ggplot2::theme(panel.background = ggplot2::element_rect(fill = "#fff5f5", color = "red", linewidth = 2))
}


## ----loss-aversion-demo-plot, fig.cap ="Illustration of loss aversion as a consequence of magnitude processing. (a) Subjective utility as a function of relative gains or losses with respect to an endowment, for different values of the uncertainty parameter $w$. The S-shaped curve emerges naturally from the asymmetry of the Weber ratios for gains and losses. Dotted blue lines mark the utility values at a change of $\\pm 50\\%$, illustrating that the absolute utility of a $50\\%$ loss exceeds that of a $50\\%$ gain. (b) Ratio of absolute utilities for losses vs.\\ gains, $\\frac{\\left|\\text{Utility}_{\\text{Loss}}\\right|}{\\left|\\text{Utility}_{\\text{Gain}}\\right|}$, as a function of relative change. Values greater than 1 indicate loss aversion. Note that loss aversion exceeds 1 for all values of $w$ and increases with the size of the change.", fig.width=8----



ggpubr::ggarrange(
    
    dat_prospect_demo %>% 
        create_prospect_change_value_plot(),
    
    dat_prospect_demo %>% 
        create_prospect_relative_value_plot(),
    
    labels = "auto",
    common.legend = TRUE,
    legend = "bottom"
    
)



## ----add-tk-to-data------------------------------------------------------------------------------

w_tk_best <- create_data_for_prospect_demo(
    100, 
    ws = seq(.1, 10, .1)
) %>% 
    dplyr::group_by(w) %>% 
    dplyr::group_modify(~ add_tk_data(.x)) %>% 
    # Still grouped
    dplyr::summarize(
        SS = sum((value - value_tk_scaled)^2),
        .groups = "drop"
    ) %>% 
    dplyr::slice_min(SS) %>% 
    dplyr::pull(w) %>% 
    as.numeric()

message("Best fit is ", w_tk_best)

dat_prospect_demo_tk <- create_data_for_prospect_demo(
    100,
    ws = w_tk_best
) %>% 
    add_tk_data() %>% 
    dplyr::select(-value_tk, -change, -ratio) %>% 
    tidyr::pivot_longer(cols = c(value, value_tk_scaled),
                        names_to = "Model",
                        values_to = "value"
    ) %>% 
    dplyr::mutate(Model = dplyr::if_else(str_detect(Model, "tk"),
                                         "TK1992",
                                         "EA"))





## ----tk-plot, fig.cap = "Comparison of the current model (EA) and the Tversky and Kahneman (1992) model, with parameterization $\\lambda = 2.25$, $\\alpha = \\beta = 0.88$. The $w$ parameter was set to a large value ($w = 10$) to approximate the quasi-linear behavior of the Tversky and Kahneman (1992) function. Because both models use arbitrary units, the output of the Tversky and Kahneman (1992) model was scaled so that the maxima of both functions coincide. The two models produce closely overlapping value functions across the full range of relative changes."----

dat_prospect_demo_tk %>% 
    ggplot(aes(x = relative_change, y = value, group = Model, col = Model, lty = Model)) +
    geom_line() +
    labs(
        x = "Relative change (relative to endowment)",
        y = "Utility (arbitrary units)"
    ) +
    ggh4x::coord_axes_inside(
        xintercept = 0, 
        yintercept = 0,
        labels_inside = TRUE,
    ) 



## ----print-exclusions----------------------------------------------------------------------------
list(
    incomplete = dat.incomplete.subj,
    no_variability = dat.invariable.subj,
    too_fast = dat.fast.subj,
    attention_check = dat.moral.numbers.failed.attention.check,
    vivacity_severity = dat.moral.numbers.bad.subj.severity,
    vivacity_acceptability = dat.moral.numbers.vivacity.bad.subj.acceptability
) %>%
    purrr::list_rbind(names_to = "criterion") %>%
    dplyr::select(criterion, experimentID, ResponseId) %>%
    dplyr::filter(
        # Filter only those experiments that are actually used and reformat the IDs
        experimentID %in% dat.moral.numbers.exp.correspondance$experimentID.data,
        # This is the asymptotic search experiment
        experimentID != "exp9a"
    ) %>%
    combine_exps("exp2[ac]") %>%
    format_exp_info() %>%
    # Keep only the FIRST time a ResponseId appears (respecting list order)
    # .keep_all = TRUE ensures the 'criterion' column stays attached to the row
    dplyr::distinct(experimentID, ResponseId, .keep_all = TRUE) %>%
    # Get the number of exclusions by criterion and experiment
    dplyr::group_by(criterion, experimentID) %>%
    dplyr::summarise(
        N = dplyr::n(),
        .groups = "drop"
    ) %>%
    # Pivot wider in the expected order
    dplyr::mutate(
        criterion = factor(
            criterion,
            levels = c("incomplete", "no_variability", "too_fast", "attention_check", "vivacity_severity", "vivacity_acceptability")
        )
    ) %>%
    dplyr::arrange(criterion, experimentID) %>%
    tidyr::pivot_wider(
        names_from = criterion,
        values_from = N
    ) %>%
    # Calculate total exclusions per experiment
    dplyr::rowwise() %>%
    dplyr::mutate(
        dplyr::across(
            dplyr::where(is.numeric),
            \(x) tidyr::replace_na(x, 0)
        ),
        TOTAL = sum(
            dplyr::c_across(dplyr::where(is.numeric)),
            na.rm = TRUE
        )
    ) %>%
    dplyr::ungroup() %>% 
    dplyr::arrange(experimentID) %>%
    knitr::kable(
        caption = "Number of participants excluded per experiment per exclusion criterion. Exclusions were applied sequentially, so each participant is counted at most once (under the first criterion they failed). \\textit{Incomplete}: did not complete all trials. \\textit{No variability}: gave identical ratings to all scenarios. \\textit{Fast completion}: completion time below one quarter of the experiment median. \\textit{Attention check}: failed an embedded attention-check question (not included in all experiments). \\textit{Vivacity: severity}: in the vivid condition, victims' deaths rated as not more horrific than beneficiaries' on more than 50\\% of trials, or in the neutral condition, a significant difference in perceived severity (vivacity experiments only). \\textit{Vivacity: acceptability}: vivid scenarios rated as more acceptable than neutral scenarios on average (vivacity experiments only, applied after the severity criterion). \\textit{Total}: unique excluded participants.",
        booktabs = TRUE,
        col.names = c("Experiment", "Incomplete", "No variability", "Fast completion", "Attention check", "Vivacity: severity", "Vivacity: acceptability", "Total")
    ) %>%
    kableExtra::kable_classic() %>%
    kableExtra::kable_styling(
        #  font_size = 9,
        latex_options = c(
            "scale_down",
            "hold_position",
            "striped"
        )
    )


## ----plot-completion-durations, fig.height=9, fig.width=9, fig.cap="Distribution of completion durations in minutes per experiment, shown for the final sample after all exclusions. Participants whose completion time was less than one quarter of the median for their experiment were excluded."----
# In Experiment 5b, we considered calculating the durations separately for each condition,
# as the text is longer in the vivid condition. However, given that the duration difference between
# the conditions is less than 2 min in total, we are unlikely to exclude participants based on the overall condition.

dat.moral.numbers %>%
    dplyr::filter(
        experimentID %in% dat.moral.numbers.exp.correspondance$experimentID.data
    ) %>%
    combine_exps("exp2[ac]") %>%
    # This is the asymptotic search experiment
    dplyr::filter(experimentID != "exp9a") %>%
    format_exp_info() %>%
    ggplot2::ggplot(ggplot2::aes(x = duration)) +
    ggplot2::geom_histogram(ggplot2::aes(y = ..density..), color = "#0000AA", fill = "#0000AA") +
    ggplot2::geom_density(alpha = .8, fill = "#6666FF") +
    ggplot2::labs(x = "Run duration (minutes)") +
    ggplot2::facet_wrap(~experimentID, scales = "free")


## ----calculate-demographics-vivacity-working-----------------------------------------------------
dat.moral.numbers.demographics.vivacity_working <- dat.moral.numbers %>%
    dplyr::ungroup() %>%
    dplyr::mutate(gender = dplyr::case_when(
        gender == 1 ~ "Male",
        gender == 2 ~ "Female",
        gender == 3 ~ "Other",
        gender == 4 ~ "Undisclosed",
        TRUE ~ NA_character_
    )) %>%
    get.demographics2(ResponseId, gender, age, experimentID)


## ----print-demographics--------------------------------------------------------------------------
dplyr::bind_rows(
    dat.moral.numbers.demographics,
    dat.moral.numbers.demographics.vivacity_working %>%
        dplyr::filter(
            stringr::str_detect(experimentID, "vivacityManipulationWorking")
        )
) %>%
    {dplyr::bind_rows(
        dplyr::filter(.,
                      !stringr::str_detect(experimentID, "exp[3567]"),
                      !stringr::str_detect(experimentID, "exp2[ac]")
        ),
        dplyr::filter(., 
                      stringr::str_detect(experimentID, "exp2[ac]")
        ) %>%
            # Combining Experiments 2a and 2c as they are replications of one another
            combine_exps("exp2[ac]") %>%
            combine_demographics()
    )} %>%
    format_exp_info() %>%
    dplyr::arrange(experimentID) %>%
    knitr::kable(
        caption = "Participant demographics. Experiment AS (Asymptote Search) was an unsuccessful attempt to identify the asymptotic range using vivid scenarios with more extreme beneficiary:victim ratios (up to 1:20,000); see SI \\ref{app:exp-AS} for details. For Experiments 4--6, rows marked (filtered) show the subset of participants remaining after vivacity-related exclusions (see SI \\ref{app:participants} for general exclusion criteria and SI \\ref{app:vivacity-verifications} for vivacity-specific exclusion criteria).",
        booktabs = TRUE,
        col.names = c("Experiment", "N", "Females", "Males", "Other", "Age", "Age range")
    ) %>%
    kableExtra::kable_classic()


## ----print-trials-per-exp------------------------------------------------------------------------
TRIALS_INFO %>%
    {dplyr::bind_rows(
        dplyr::filter(.,
                      !stringr::str_detect(experimentID, "exp[3567]"),
                      !stringr::str_detect(experimentID, "exp2[ac]")
        ),
        dplyr::filter(.,
                      stringr::str_detect(experimentID, "exp2[ac]")
        ) %>%
            combine_exps("exp2[ac]") %>%
            dplyr::distinct()
    )} %>%
    dplyr::mutate(
        conditions = dplyr::if_else(
            is.na(block_type),
            as.character(n_blocks),
            stringr::str_c(n_blocks, " (", block_type, ")")
        )
    ) %>%
    dplyr::select(experimentID, n_scenarios_per_block, conditions) %>%
    format_exp_info() %>%
    dplyr::arrange(experimentID) %>%
    knitr::kable(
        caption = "Within-subjects block structure across experiments. \\textit{Scenarios / block} is the number of unique scenario--number-condition combinations presented in each block. \\textit{Blocks (contrast)} is the number of within-subjects blocks; for experiments with more than one block, the contrast between blocks is shown in parentheses (moral vs.\\ economic decisions, or vivid vs.\\ neutral victim descriptions).",
        booktabs = TRUE,
        col.names = c("Experiment", "Scenarios / block", "Blocks (contrast)")
    ) %>%
    kableExtra::kable_classic()


## ----print-neutral-vivid-scenario-example--------------------------------------------------------

list(
    `Neutral version` =
r"(Settlements in the Burning Barren desert are supplied with water through a series of aqueducts. The supply to Sandharbor settlement has \textcolor{red}{\textit{a leak}}.

All of the people in Sandharbor will die if nothing is done. There are few options for relief efforts, as there are no off-road tanker trucks and any evacuation would take too much time because both communities are remote and inaccessible. However, the control engineer can remotely reroute the water supply for the Midcross settlement to Sandharbor. This would \textcolor{red}{\textit{provide water to Sandharbor}}, but kill all of the people in Midcross.

\textcolor{red}{\textit{If the control engineer switches}} the supply from Midcross to Sandharbor, this will save all of the $<$\# beneficiaries$>$ people in Sandharbor, but all of the $<$\# victims$>$ people in Midcross will \textcolor{red}{\textit{die as a result}}.)",

    `Vivid version` =
r"(Settlements in the Burning Barren desert are supplied with water through a series of aqueducts. The supply to Sandharbor settlement has \textcolor{red}{\textit{been poisoned with Possuxyth, a potent, fast-acting but relatively pain-free poison}}.

All of the people in Sandharbor will die if nothing is done. There are few options for relief efforts, as there are no off-road tanker trucks and any evacuation would take too much time because both communities are remote and inaccessible. However, the control engineer can remotely reroute the water supply from the Midcross settlement to Sandharbor. This would \textcolor{red}{\textit{dilute the poison enough to make the water safe to drink}}, but kill all of the people in Midcross.

\textcolor{red}{\textit{You are the control engineer. If you switch}} the supply from Midcross to Sandharbor, you will save all of the $<$\# beneficiaries$>$ people in Sandharbor, but all of the $<$\# victims$>$ people in Midcross will \textcolor{red}{\textit{endure a painful death due to dehydration, which can take up to a week}}.)") %>%
    tibble::as_tibble_row() %>%
    dplyr::mutate(
        dplyr::across(
            dplyr::everything(), 
            ~ kableExtra::linebreak(.x, align = "l")
        )
    ) %>%
    knitr::kable(
        caption = "Example scenario pair illustrating the neutral and vivid versions of the aqueduct dilemma. Text in red italics marks passages that differ between the two versions; no such highlighting was used in the actual experiment.",
        booktabs = TRUE,
        escape = FALSE,
        align = "l"
    ) %>%
    kableExtra::kable_styling(
        latex_options = c("hold_position"),
        full_width = FALSE
    ) %>%
    kableExtra::column_spec(1, width = "7.5cm") %>%
    kableExtra::column_spec(2, width = "7.5cm") %>%
    kableExtra::kable_classic_2()


## ----descriptives-print2, ref.label = 'descriptives-print', echo = FALSE, results = 'markup'-----


## ----exp1-model-comp-print-app, ref.label='exp1-model-comp-print', echo=FALSE--------------------


## ----fits-bootstrap-with-forced-w-print-app, ref.label='fits-bootstrap-with-forced-w-print', echo=FALSE----


## ----bootstrap-fits-print-app, ref.label='bootstrap-fits-print', echo=FALSE----------------------


## ----exp2-myopic-proportions, fig.cap = "Test of the myopicity of the participant responses in Experiment 2. Participants read a scenario with (1) an option where 300 people are sacrificed to save 400 people, and (2) an option where 3000 people would be sacrificed to save 4000, and were asked how many people would be saved and how many would be sacrificed if the first option is chosen. A myopic evaluation predicts that participants report 400 saved and 300 sacrificed, while integrating numbers across options would predict 3400 saved and 4300 sacrificed. The overwhelming majority of participants gave the myopic answer, indicating that they evaluated options separately rather than integrating numbers across options.", fig.height = 9----
dat.moral.numbers %>%
    dplyr::filter(stringr::str_starts(experimentID, "exp2")) %>%
    # Combining Experiments 2a and 2c as they are replications of one another
    combine_exps("exp2[ac]") %>%
    dplyr::select(experimentID, ResponseId, myopic.n.saved, myopic.n.victims) %>%
    dplyr::distinct() %>%
    dplyr::select(-ResponseId) %>%
    tidyr::pivot_longer(
        cols = -experimentID,
        names_to = "question",
        values_to = "answer"
    ) %>%
    dplyr::group_by(experimentID, question, answer) %>%
    dplyr::summarize(N = dplyr::n()) %>%
    dplyr::mutate(n.total = sum(N)) %>%
    dplyr::mutate(question = dplyr::recode(question,
                                           "myopic.n.saved"   = "Number of saved",
                                           "myopic.n.victims" = "Number of victims"
    )) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(comparisonType = ifelse(experimentID == "exp2b",
                                          "ratio_vs_victims",
                                          "ratio_vs_utility"
    )) %>%
    format_exp_info(wrap = TRUE) %>%
    dplyr::mutate(p = 100 * N / n.total) %>%
    dplyr::select(-n.total) %>%
    ggplot2::ggplot(ggplot2::aes(x = factor(answer), y = p)) +
    #    theme_linedraw(14) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::labs(
        x = ggplot2::element_blank(),
        y = "% Responses"
    ) +
    ggplot2::scale_x_discrete( # "Answer",
        # labels = ~ stringr::str_wrap(.x, 15),
        guide = guide_axis(angle = 60)
    ) +
    ggplot2::facet_grid(experimentID ~ question, scales = "free_x") +
    ggplot2::theme(plot.title = ggplot2::element_text(hjust = 0.5)) +
    ggplot2::labs(title = "Participant estimate of the")


## ----exp2-descriptives-print2, ref.label = 'exp2-descriptives-print', echo = FALSE, results = 'markup'----


## ----glmer-print2, ref.label = 'glmer-print', echo = FALSE, results = 'markup'-------------------


## ----exp2-lmer-print2, ref.label = 'exp2-lmer-print', echo = FALSE, results = 'markup'-----------


## ----parameter-recovery-define-functions, include = FALSE----------------------------------------
# Functions moved to helper_functions/moral_numbers_parameter_recovery.R


## ----parameter-recovery-define-parameters--------------------------------------------------------
N_SIM_RECOVER <- 1000
N_SUBJ_RECOVER <- 100
ratios <- c(4 / 3, 2, 3, 4, 10, 15, 20, 50, 100, 500, 2000, 10000)


# from neutral condition in exp 8 & 10
ws <- dat.moral.numbers.bootstrap.1param.summary %>%
    dplyr::ungroup() %>%
    dplyr::filter(
        stringr::str_detect(experimentID, "vivacityManipulationWorking"),
        !stringr::str_detect(experimentID, "exp9"),
        blocks == "all",
        vivacity == "neutral"
    ) %>%
    dplyr::select(w_M, w_SD) %>%
    rnorm_mixture(N_SUBJ_RECOVER, dat_mu_sigma = .)

# from vivid condition in exp 8 & 10
as <- dat.moral.numbers.bootstrap.2param.summary %>%
    dplyr::ungroup() %>%
    dplyr::filter(
        stringr::str_detect(experimentID, "vivacityManipulationWorking"),
        !stringr::str_detect(experimentID, "exp9"),
        blocks == "all",
        vivacity == "neutral"
    ) %>%
    dplyr::select(a_M, a_SD) %>%
    rnorm_mixture(N_SUBJ_RECOVER, dat_mu_sigma = .)


## ----parameter-recovery-run, eval = FALSE--------------------------------------------------------
# dat_parameter_recovery <- purrr::map_dfr(
#     1:N_SIM_RECOVER, function(current_sim) {
#         # Generate simulated responses
#         dat_mean_responses <- purrr::map_dfr(1:N_SUBJ_RECOVER, function(current_subj) {
#             data.frame(
#                 subj = current_subj,
#                 ratio = ratios,
#                 neutral = purrr::map_int(
#                     acceptability.fnc(w = ws[current_subj], n1 = 1, ratio = ratios),
#                     ~ rbinom(1, 1, .x)
#                 ),
#                 vivid = purrr::map_int(
#                     acceptability.fnc(w = ws[current_subj], a = as[current_subj], n1 = 1, ratio = ratios),
#                     ~ rbinom(1, 1, .x)
#                 )
#             )
#         }) %>%
#             dplyr::group_by(ratio) %>%
#             dplyr::summarise(dplyr::across(c(neutral, vivid), mean, na.rm = TRUE), .groups = "drop")
# 
#         # Fit models
#         fit_neutral_w <- fit_with_nlsLM(
#             neutral ~ acceptability.fnc(w, n1 = 1, ratio = ratios),
#             data = dat_mean_responses,
#             start = FIT_PARAMS$start["w"],
#             lower = FIT_PARAMS$lower["w"],
#             upper = FIT_PARAMS$upper["w"]
#         ) %>%
#             dplyr::mutate(fit_type = "w", .before = 1)
# 
#         fit_vivid_w_a <- fit_with_nlsLM(
#             vivid ~ acceptability.fnc(w, n1 = 1, ratio = ratios, a = a),
#             data = dat_mean_responses,
#             start = FIT_PARAMS$start,
#             lower = FIT_PARAMS$lower,
#             upper = FIT_PARAMS$upper
#         ) %>%
#             dplyr::mutate(fit_type = "w_a", .before = 1)
# 
#         w_neutral <- fit_neutral_w %>%
#             dplyr::filter(term == "w") %>%
#             dplyr::pull(estimate)
# 
#         fit_vivid_a_only <- fit_with_nlsLM(
#             vivid ~ acceptability.fnc(w = w_neutral, n1 = 1, ratio = ratios, a = a),
#             data = dat_mean_responses,
#             start = FIT_PARAMS$start["a"],
#             lower = FIT_PARAMS$lower["a"],
#             upper = FIT_PARAMS$upper["a"],
#             control = minpack.lm::nls.lm.control(
#                 maxiter = 1000, ftol = 1e-8, ptol = 1e-8, gtol = 1e-8, factor = 100
#             )
#         ) %>% dplyr::mutate(fit_type = "a_only", .before = 1)
# 
#         # Combine results
#         dplyr::bind_rows(fit_neutral_w, fit_vivid_w_a, fit_vivid_a_only) %>%
#             dplyr::mutate(sim = current_sim, .before = 1)
#     }
# ) %>%
#     # Keep only full fits (i.e., 4 fit types per sim: w, w_a (2 rows), a_only)
#     dplyr::group_by(sim) %>%
#     dplyr::filter(dplyr::n() == 4) %>%
#     dplyr::ungroup()


## ----parameter-recovery-print, eval = FALSE------------------------------------------------------
# dplyr::bind_rows(
#     dat.moral.numbers.bootstrap.1param.summary %>%
#         dplyr::ungroup() %>%
#         dplyr::filter(
#             stringr::str_detect(experimentID, "vivacityManipulationWorking"),
#             !stringr::str_detect(experimentID, "exp9"),
#             blocks == "all",
#             vivacity == "neutral"
#         ) %>%
#         dplyr::select(experimentID, w_M, w_SD) %>%
#         dplyr::rename(fit_type = experimentID) %>%
#         dplyr::mutate(term = "w", .after = fit_type) %>%
#         dplyr::rename_with(~ stringr::str_remove(.x, "^._"), cols = dplyr::matches("_(M|SD)$")),
#     dat.moral.numbers.bootstrap.2param.summary %>%
#         dplyr::ungroup() %>%
#         dplyr::filter(
#             stringr::str_detect(experimentID, "vivacityManipulationWorking"),
#             !stringr::str_detect(experimentID, "exp9"),
#             blocks == "all",
#             vivacity == "neutral"
#         ) %>%
#         dplyr::select(experimentID, a_M, a_SD) %>%
#         dplyr::rename(fit_type = experimentID) %>%
#         dplyr::mutate(term = "a", .after = fit_type) %>%
#         dplyr::rename_with(~ stringr::str_remove(.x, "^._"), cols = dplyr::matches("_(M|SD)$")),
#     dat_parameter_recovery %>%
#         dplyr::group_by(fit_type, term) %>%
#         dplyr::summarise(
#             M = mean(estimate, na.rm = TRUE),
#             SD = sd(estimate, na.rm = TRUE)
#         )
# ) %>%
#     dplyr::mutate(fit_type = factor(fit_type,
#                                     levels = c(
#                                         "exp8.vivacityManipulationWorking",
#                                         "exp10.vivacityManipulationWorking",
#                                         "w", "w_a", "a_only"
#                                     )
#     )) %>%
#     dplyr::arrange(fit_type, desc(term)) %>%
#     kable.packed(
#         "fit_type",
#         caption = "Parameter recovery simulations",
#         col.names = en_math_col_names(., 1),
#         booktabs = TRUE,
#         longtable = TRUE,
#         escape = FALSE
#     ) %>%
#     kableExtra::kable_styling(latex_options = c(
#         "striped",
#         "scale_down",
#         "hold_position",
#         "repeat_header"
#     )) %>%
#     kableExtra::kable_classic_2()


## ----exp8-severity-plot-by-subjects-scenarios-raw, fig.cap = "Relative horrificness ratings in Experiment 4 (was 8) for the deaths of the victims and the beneficiaries, respectively. (a) Averages by participants. (b) Differences between the vivid and the neutral condition by participants. (c) Averages by scenarios. (d) Differences between the vivid and the neutral condition by scenarios.", fig.height=12, out.height="0.7\\textheight", out.extra="keepaspectratio"----
ggpubr::ggarrange(
    dat.moral.numbers.severity.m.by.subj %>%
        dplyr::filter(experimentID == "exp8") %>%
        format_exp_info(wrap = TRUE) %>%
        ggplot2::ggplot(ggplot2::aes(x = vivacity, y = rating)) %>%
        violin_plot_template(yintercept = 3.5) +
        ggplot2::scale_y_continuous("Relative Horrificness (Victim > Beneficiary)") + # , limits = 0:1) +
        ggplot2::labs(
            x = "Vivacity",
            title = "By participants"
        ),
    dat.moral.numbers.severity.m.by.subj %>%
        dplyr::filter(experimentID == "exp8") %>%
        tidyr::pivot_wider(
            id_cols = c(experimentID, first.scenario.type, ResponseId),
            names_from = vivacity,
            values_from = rating
        ) %>%
        dplyr::mutate(d = vivid - neutral) %>%
        ggplot2::ggplot(ggplot2::aes(x = 1, y = d)) %>%
        violin_plot_template(yintercept = 0) +
        ggplot2::scale_y_continuous("Difference in Relative Horrificness (Vivid > Neutral)") + # , limits = 0:1) +
        ggplot2::labs(
            x = ggplot2::element_blank(),
            title = "By participants"
        ) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
        ),
    dat.moral.numbers.severity.m.by.scenarios %>%
        dplyr::filter(experimentID == "exp8") %>%
        format_exp_info(wrap = TRUE) %>%
        ggplot2::ggplot(ggplot2::aes(x = vivacity, y = rating)) %>%
        violin_plot_template(yintercept = 3.5) +
        ggplot2::scale_y_continuous("Relative Horrificness (Victim > Beneficiary)") + # , limits = 0:1) +
        ggplot2::labs(
            x = "Vivacity",
            title = "By scenarios"
        ) +
        ggplot2::geom_label(ggplot2::aes(label = scenarioID)),
    dat.moral.numbers.severity.m.by.scenarios %>%
        dplyr::filter(experimentID == "exp8") %>%
        tidyr::pivot_wider(
            id_cols = c(experimentID, scenarioID),
            names_from = vivacity,
            values_from = rating
        ) %>%
        dplyr::mutate(d = vivid - neutral) %>%
        ggplot2::ggplot(ggplot2::aes(x = 1, y = d)) %>%
        violin_plot_template(yintercept = 0) +
        ggplot2::scale_y_continuous("SI Difference in Relative Horrificness (Vivid > Neutral)") + # , limits = 0:1) +
        ggplot2::labs(
            x = NULL,
            title = "By scenarios"
        ) +
        ggplot2::geom_label(ggplot2::aes(label = scenarioID)) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
        ),
    ncol = 2, nrow = 2, labels = "auto",
    widths = c(1.5, 1),
    common.legend = TRUE
)


## ----exp8-plot-severity-proportion-of-positive-ratings-per-subject, fig.cap='Proportions of relative horrificness rating above, at or below 3.5 (i.e., the midline of the scale) in Experiment 4 (was 8).'----
dat.moral.numbers.severity.m.by.subj %>%
    dplyr::filter(experimentID == "exp8") %>%
    # dplyr::filter(vivacity == "vivid") %>%
    format_exp_info(wrap = TRUE) %>%
    dplyr::group_by(vivacity) %>%
    dplyr::mutate(ResponseId_vivacity = paste(ResponseId, vivacity, sep = "_")) %>%
    dplyr::mutate(ResponseId_vivacity = reorder(ResponseId_vivacity, -p.victims.greater.saved)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(
        x = ResponseId_vivacity,
        # x = reorder(ResponseId, -p.victims.greater.saved),
        y = p.victims.greater.saved, fill = first.scenario.type
    )) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_y_continuous("SI Proportion of relative severity ratings above 3.5") + # , limits = 0:1) +
    ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
        x = "Participant",
        fill = "First scenario type"
    ) +
    ggplot2::facet_wrap(. ~ vivacity, labeller = labeller(first.scenario.type = label_both), scales = "free_x")


## ----exp10-severity-plot-by-subjects-scenarios-raw, fig.cap = "Relative horrificness ratings in Experiment 5 for the deaths of the victims and the beneficiaries, respectively. (a) Averages by participants. (b) Differences between the vivid and the neutral condition by participants. (c) Averages by scenarios. (d) Differences between the vivid and the neutral condition by scenarios.", fig.height=12, out.height="0.7\\textheight", out.extra="keepaspectratio"----
ggpubr::ggarrange(
    dat.moral.numbers.severity.m.by.subj %>%
        dplyr::filter(experimentID == "exp10") %>%
        format_exp_info(wrap = TRUE) %>%
        # dplyr::mutate(experimentID = stringr::str_to_sentence(experimentID)) %>%
        # dplyr::mutate(experimentID = stringr::str_replace(experimentID, "victims", "harm")) %>%
        # dplyr::mutate(vivacity = stringr::str_to_title(vivacity)) %>%
        ggplot2::ggplot(ggplot2::aes(x = vivacity, y = rating)) %>%
        violin_plot_template(yintercept = 3.5) +
        ggplot2::scale_y_continuous("Relative Horrificness (Victim > Beneficiary)") + # , limits = 0:1) +
        ggplot2::labs(
            x = "Vivacity",
            title = "By participants"
        ),
    dat.moral.numbers.severity.m.by.subj %>%
        dplyr::filter(experimentID == "exp10") %>%
        tidyr::pivot_wider(
            id_cols = c(experimentID, first.scenario.type, ResponseId),
            names_from = vivacity,
            values_from = rating
        ) %>%
        dplyr::mutate(d = vivid - neutral) %>%
        ggplot2::ggplot(ggplot2::aes(x = 1, y = d)) %>%
        violin_plot_template(yintercept = 0) +
        ggplot2::scale_y_continuous("Difference in Relative Horrificness (Vivid > Neutral)") + # , limits = 0:1) +
        ggplot2::labs(
            x = ggplot2::element_blank(),
            title = "By participants"
        ) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
        ),
    dat.moral.numbers.severity.m.by.scenarios %>%
        dplyr::filter(experimentID == "exp10") %>%
        format_exp_info(wrap = TRUE) %>%
        ggplot2::ggplot(ggplot2::aes(x = vivacity, y = rating)) %>%
        violin_plot_template(yintercept = 3.5) +
        ggplot2::scale_y_continuous("Relative Horrificness (Victim > Beneficiary)") + # , limits = 0:1) +
        ggplot2::labs(
            x = "Vivacity",
            title = "By scenarios"
        ) +
        ggplot2::geom_label(ggplot2::aes(label = scenarioID)),
    dat.moral.numbers.severity.m.by.scenarios %>%
        dplyr::filter(experimentID == "exp10") %>%
        tidyr::pivot_wider(
            id_cols = c(experimentID, scenarioID),
            names_from = vivacity,
            values_from = rating
        ) %>%
        dplyr::mutate(d = vivid - neutral) %>%
        ggplot2::ggplot(ggplot2::aes(x = 1, y = d)) %>%
        violin_plot_template(yintercept = 0) +
        ggplot2::scale_y_continuous("Difference in Relative Horrificness (Vivid > Neutral)") + # , limits = 0:1) +
        ggplot2::labs(
            x = NULL,
            title = "By scenarios"
        ) +
        ggplot2::geom_label(ggplot2::aes(label = scenarioID)) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
        ),
    ncol = 2, nrow = 2, labels = "auto",
    widths = c(1.5, 1),
    common.legend = TRUE
)


## ----exp10-plot-severity-proportion-of-positive-ratings-per-subject, fig.cap='Proportions of relative horrificness rating above, at or below 3.5 (i.e., the midline of the scale), in Experiment 5'----
dat.moral.numbers.severity.m.by.subj %>%
    dplyr::filter(experimentID == "exp10") %>%
    # dplyr::filter(vivacity == "vivid") %>%
    format_exp_info(wrap = TRUE) %>%
    dplyr::mutate(ResponseId_vivacity = paste(ResponseId, vivacity, sep = "_")) %>%
    dplyr::mutate(ResponseId_vivacity = reorder(ResponseId_vivacity, -p.victims.greater.saved)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(
        x = ResponseId_vivacity,
        # x = reorder(ResponseId, -p.victims.greater.saved),
        y = p.victims.greater.saved, fill = first.scenario.type
    )) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_y_continuous("Proportion of relative severity ratings above 3.5") + # , limits = 0:1) +
    ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
        x = "Participant",
        fill = "First scenario type"
    ) +
    ggplot2::facet_wrap(. ~ vivacity, labeller = labeller(first.scenario.type = label_both), scales = "free_x")


## ----exp11-severity-plot-by-subjects-scenarios-raw, fig.cap = "Relative horrificness ratings in Experiment 6 for the deaths of the victims and the beneficiaries, respectively. (a) Averages by participants. (b) Differences between the vivid and the neutral condition by participants. (c) Averages by scenarios. (d) Differences between the vivid and the neutral condition by scenarios.", fig.height=12, out.height="0.7\\textheight", out.extra="keepaspectratio"----
ggpubr::ggarrange(
    dat.moral.numbers.severity.m.by.subj %>%
        dplyr::filter(experimentID == "exp11") %>%
        format_exp_info(wrap = TRUE) %>%
        # dplyr::mutate(experimentID = stringr::str_to_sentence(experimentID)) %>%
        # dplyr::mutate(experimentID = stringr::str_replace(experimentID, "victims", "harm")) %>%
        # dplyr::mutate(vivacity = stringr::str_to_title(vivacity)) %>%
        ggplot2::ggplot(ggplot2::aes(x = vivacity, y = rating)) %>%
        violin_plot_template(yintercept = 3.5) +
        ggplot2::scale_y_continuous("Relative Horrificness (Victim > Beneficiary)") + # , limits = 0:1) +
        ggplot2::labs(
            x = "Vivacity",
            title = "By participants"
        ),
    dat.moral.numbers.severity.m.by.subj %>%
        dplyr::filter(experimentID == "exp11") %>%
        tidyr::pivot_wider(
            id_cols = c(experimentID, first.scenario.type, ResponseId),
            names_from = vivacity,
            values_from = rating
        ) %>%
        dplyr::mutate(d = vivid - neutral) %>%
        ggplot2::ggplot(ggplot2::aes(x = 1, y = d)) %>%
        violin_plot_template(yintercept = 0) +
        ggplot2::scale_y_continuous("Difference in Relative Horrificness (Vivid > Neutral)") + # , limits = 0:1) +
        ggplot2::labs(
            x = ggplot2::element_blank(),
            title = "By participants"
        ) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
        ),
    dat.moral.numbers.severity.m.by.scenarios %>%
        dplyr::filter(experimentID == "exp11") %>%
        format_exp_info(wrap = TRUE) %>%
        ggplot2::ggplot(ggplot2::aes(x = vivacity, y = rating)) %>%
        violin_plot_template(yintercept = 3.5) +
        ggplot2::scale_y_continuous("Relative Horrificness (Victim > Beneficiary)") + # , limits = 0:1) +
        ggplot2::labs(
            x = "Vivacity",
            title = "By scenarios"
        ) +
        ggplot2::geom_label(ggplot2::aes(label = scenarioID)),
    dat.moral.numbers.severity.m.by.scenarios %>%
        dplyr::filter(experimentID == "exp11") %>%
        tidyr::pivot_wider(
            id_cols = c(experimentID, scenarioID),
            names_from = vivacity,
            values_from = rating
        ) %>%
        dplyr::mutate(d = vivid - neutral) %>%
        ggplot2::ggplot(ggplot2::aes(x = 1, y = d)) %>%
        violin_plot_template(yintercept = 0) +
        ggplot2::scale_y_continuous("Difference in Relative Horrificness (Vivid > Neutral)") + # , limits = 0:1) +
        ggplot2::labs(
            x = NULL,
            title = "By scenarios"
        ) +
        ggplot2::geom_label(ggplot2::aes(label = scenarioID)) +
        ggplot2::theme(
            axis.text.x = ggplot2::element_blank(),
            axis.ticks.x = ggplot2::element_blank()
        ),
    ncol = 2, nrow = 2, labels = "auto",
    widths = c(1.5, 1),
    common.legend = TRUE
)


## ----exp11-plot-severity-proportion-of-positive-ratings-per-subject, fig.cap='Proportions of relative horrificness rating above, at or below 3.5 (i.e., the midline of the scale), in Experiment 6'----
dat.moral.numbers.severity.m.by.subj %>%
    dplyr::filter(experimentID == "exp11") %>%
    # dplyr::filter(vivacity == "vivid") %>%
    format_exp_info(wrap = TRUE) %>%
    dplyr::mutate(ResponseId_vivacity = paste(ResponseId, vivacity, sep = "_")) %>%
    dplyr::mutate(ResponseId_vivacity = reorder(ResponseId_vivacity, -p.victims.greater.saved)) %>%
    dplyr::ungroup() %>%
    ggplot2::ggplot(ggplot2::aes(
        x = ResponseId_vivacity,
        # x = reorder(ResponseId, -p.victims.greater.saved),
        y = p.victims.greater.saved, fill = first.scenario.type
    )) +
    ggplot2::geom_bar(stat = "identity") +
    ggplot2::scale_y_continuous("Proportion of relative severity ratings above 3.5") + # , limits = 0:1) +
    ggplot2::theme(
        axis.text.x = ggplot2::element_blank(),
        axis.ticks.x = ggplot2::element_blank()
    ) +
    ggplot2::labs(
        x = "Participant",
        fill = "First scenario type"
    ) +
    ggplot2::facet_wrap(. ~ vivacity, labeller = labeller(first.scenario.type = label_both), scales = "free_x")


## ----create-rr-correlations-no-bootstrap-app, ref.label='create-rr-correlations-no-bootstrap', echo=FALSE----


## ----relative-risk-slope-print-app, ref.label='relative-risk-slope-print', echo=FALSE, tab.cap="Bootstrap estimates of the linear regression slope and Spearman correlation between the relative risk ($\\RR = P_{\\text{acceptable, vivid}} / P_{\\text{acceptable, neutral}}$) and the \\textit{beneficiary:victim} ratio in Experiments 4 to 6 (10,000 bootstrap samples). Each bootstrap sample yields average acceptance rates per ratio and condition; we then derive $\\RR$ and regress it against the \\textit{beneficiary:victim} ratio. The table reports the mean estimate, its Monte Carlo (MC) error, the $z$-score, the two-tailed $p$-value, and the 95\\% percentile interval (PI) for both the regression slope and the Spearman correlation."----


## ----rr-vs-weber-ratio-and-alpha-bootstrap-unfiltered-plot, fig.cap = "Relative risk of acceptability ($\\RR = P_{\\text{acceptable, vivid}} / P_{\\text{acceptable, neutral}}$, left panels) and bootstrap distribution of the attentional weight of victims parameter ($\\alpha$, right panels) in Experiments 4 to 6, including all participants regardless of their responses on the severity question. In the left panels, circles show bootstrap means and error bars show 95\\% percentile intervals (10,000 bootstrap samples); the line shows a linear regression fit and the shaded area its 95\\% confidence interval. The $w$ parameter is fixed to the bootstrap estimate from the neutral condition. The horizontal line marks $\\alpha = 1$ (no shift in attentional weight). Compare with Figure~\\ref{fig:rr-vs-weber-ratio-and-alpha-bootstrap-filtered-plot} for results restricted to participants for whom the vividness manipulation was effective.", fig.width = 8, fig.height = 13, out.height = "0.7\\textheight", out.extra = "keepaspectratio"----

# Pair RR and alpha plots per experiment, stripping individual titles and borders
purrr::map2(
    l_moral_numbers_rr_plot_unfiltered,
    l_plots_fits_bootstrap_a_with_forced_w %>%
        purrr::keep_at(\(p) !stringr::str_detect(p, "vivacityManipulationWorking")),
    \(rr, a) {
        list(
            rr,
            a
        ) %>%
            purrr::map(\(x) x +
                           ggplot2::labs(title = NULL) +
                           ggplot2::theme(plot.background = ggplot2::element_rect(colour = NA, fill = NA))
            )
    }
) %>%
    # Arrange each pair side by side, add experiment name as row title, and border the group
    # (map2 inherits names from first argument, used by imap as `name`)
    purrr::imap(\(pair, name) {
        p <- ggpubr::ggarrange(
            plotlist = pair,
            ncol = 2,
            nrow = 1,
            widths = c(2, 1)
        ) %>%
            ggpubr::annotate_figure(
                top = ggpubr::text_grob(format_exp_info(name), face = "bold")
            )
        cowplot::ggdraw(p) +
            ggplot2::theme(plot.background = ggplot2::element_rect(colour = "black", fill = NA, linewidth = 1))
    }) %>%
    # Stack rows for all experiments into a single figure
    ggpubr::ggarrange(
        plotlist = .,
        ncol = 1,
        nrow = 3,
        labels = "auto",
        common.legend = FALSE,
        legend = "bottom"
    )



## ----exp8-11-plot-filtered-bin-app, ref.label='exp8-11-plot-filtered-bin', echo=FALSE, fig.cap = "Binarized acceptability ratings for Experiments 4, 5, and 6 as a function of the \\emph{beneficiary:victim} ratio. Filtered panels show only participants for whom the vivacity manipulation was successful (see main text for exclusion criteria); unfiltered panels show all participants. Error bars represent 95\\% bootstrap confidence intervals. Model fits are based on the average response of all participants.", fig.height = 12, fig.width = 10, out.height = "0.6\\textheight", out.extra = "keepaspectratio"----


## ----exp8-plot-filtered-app, ref.label='exp8-plot-filtered', echo=FALSE, fig.cap = "Results of Experiment 4, where participants were filtered based on their responses on the severity question. Error bars represent 95\\% bootstrap confidence intervals. Model fits are based on the average response of all participants. (a) Binarized - both blocks, (b) Binarized - first block, (c) Raw - both blocks, (d) Raw - first block", fig.height=12, fig.width=10, out.height="0.6\\textheight", out.extra="keepaspectratio", eval = FALSE----
# NA


## ----exp8-plot-unfiltered-app, ref.label='exp8-plot-unfiltered', echo=FALSE, fig.cap = "Results of Experiment 4 from all participants. Error bars represent 95\\% bootstrap confidence intervals. Model fits are based on the average response of all participants. (a) Binarized - both blocks, (b) Binarized - first block, (c) Raw - both blocks, (d) Raw - first block", fig.height=12, fig.width=10, out.height="0.6\\textheight", out.extra="keepaspectratio", eval = FALSE----
# NA


## ----exp10-plot-filtered-app, ref.label='exp10-plot-filtered', echo=FALSE, fig.cap = "Results of Experiment 5, where participants were filtered based on their responses on the severity question. Error bars represent 95\\% bootstrap confidence intervals. Model fits are based on the average response of all participants. (a) Binarized - both blocks, (b) Binarized - first block, (c) Raw - both blocks, (d) Raw - first block", fig.height=12, fig.width=10, out.height="0.6\\textheight", out.extra="keepaspectratio", eval = FALSE----
# NA


## ----exp10-plot-unfiltered-app, ref.label='exp10-plot-bin-unfiltered-block12', echo=FALSE, fig.cap = "Results of Experiment 5 from all participants. Error bars represent 95\\% bootstrap confidence intervals. Model fits are based on the average response of all participants. (a) Binarized - both blocks, (b) Binarized - first block, (c) Raw - both blocks, (d) Raw - first block", fig.height=12, fig.width=10, out.height="0.6\\textheight", out.extra="keepaspectratio", eval = FALSE----
# NA


## ----exp11-plot-filtered-app, ref.label='exp11-plot-filtered', echo=FALSE, fig.cap = "Results of Experiment 6, where participants were filtered based on their responses on the severity question. Error bars represent 95\\% bootstrap confidence intervals. Model fits are based on the average response of all participants. (a) Binarized - both blocks, (b) Binarized - first block, (c) Raw - both blocks, (d) Raw - first block", fig.height=12, fig.width=10, out.height="0.6\\textheight", out.extra="keepaspectratio", eval = FALSE----
# NA


## ----exp11-plot-unfiltered-app, ref.label='exp11-plot-unfiltered', echo=FALSE, fig.cap = "Results of Experiment 6, where all participants were included. Error bars represent 95\\% bootstrap confidence intervals. Model fits are based on the average response of all participants. (a) Binarized - both blocks, (b) Binarized - first block, (c) Raw - both blocks, (d) Raw - first block", fig.height=12, fig.width=10, out.height="0.6\\textheight", out.extra="keepaspectratio", eval = FALSE----
# NA


## ----print-number-conds-exp2-2, ref.label = 'print-number-conds-exp2', echo = FALSE, results = 'markup'----


## ----print-number-conds2, ref.label = 'print-number-conds', echo = FALSE, results = 'markup'-----


## ----print-scenarios-----------------------------------------------------------------------------
dat.scenario.content %>%
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ stringr::str_replace_all(
        .x, "n\\_(\\S+\\s)",
        stringr::str_c("N", "\\1")
    ))) %>%
    dplyr::mutate(experimentID = dplyr::case_when(
        experimentID == "exp2" ~ "exp2b",
        TRUE ~ experimentID
    )) %>%
    format_exp_info() %>%
    tidyr::unite(experimentID, experimentID, condition, sep = " - ", na.rm = TRUE) %>%
    dplyr::mutate(experimentID = experimentID %>%
                      stringr::str_replace("exp", "Exp. ") %>%
                      stringr::str_remove_all("\\s*-\\s*NA\\s*")) %>%
    dplyr::arrange(experimentID) %>%
    dplyr::select(-dplyr::starts_with("option")) %>%
    kable.packed("experimentID",
                 caption = "Scenarios used in the different experiments",
                 col.names = c("Title", "Scenario", "Severity question", "Acceptability question"),
                 booktabs = TRUE,
                 longtable = TRUE,
                 escape = FALSE
    ) %>%
    kableExtra::column_spec(1, width = "2cm") %>%
    kableExtra::column_spec(2, width = "9.5cm") %>%
    kableExtra::column_spec(3, width = "3.5cm") %>%
    kableExtra::column_spec(4, width = "3.5cm") %>%
    kableExtra::kable_styling(
        font_size = 7,
        latex_options = c(
            "striped",
            "hold_position",
            "repeat_header"
        )
    ) %>%
    kableExtra::kable_classic() %>%
    kableExtra::landscape()


## ----fit-individual-calculate-1param-------------------------------------------------------------
# Just takes a few seconds, no need to parallelize
dat.moral.numbers.fit.by.subj.1param <- dat.moral.numbers.for.bootstrap.fit %>%
    dplyr::group_by(experimentID, blocks, ResponseId, decisionType, vivacity) %>%
    tidyr::nest() %>%
    dplyr::mutate(tmp.fit = purrr::map(data, ~ get.weber.ratio(.x, fit.a = FALSE, fit.w = TRUE))) %>%
    tidyr::unnest(tmp.fit) %>%
    dplyr::select(-data) %>%
    dplyr::ungroup()


## ----fit-individual-calculate-2param-------------------------------------------------------------
dat.moral.numbers.fit.by.subj.2param <- dat.moral.numbers.for.bootstrap.fit %>%
    dplyr::group_by(experimentID, blocks, ResponseId, decisionType, vivacity) %>%
    tidyr::nest() %>%
    dplyr::mutate(tmp.fit = purrr::map(data, ~ get.weber.ratio(.x, fit.a = TRUE, fit.w = TRUE))) %>%
    tidyr::unnest(tmp.fit) %>%
    dplyr::select(-data) %>%
    dplyr::ungroup()


## ----fit-individual-print, eval = FALSE----------------------------------------------------------
# dat.moral.numbers.fit.by.subj.m <- dplyr::bind_rows(
#     dat.moral.numbers.fit.by.subj.1param %>%
#         dplyr::ungroup() %>%
#         dplyr::filter(experimentID %in% c("exp1", "exp10", "exp10.vivacityManipulationWorking", "exp11", "exp11.vivacityManipulationWorking", "exp4", "exp8", "exp8.vivacityManipulationWorking")) %>%
#         dplyr::filter(is.finite(w)) %>%
#         dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
#         dplyr::summarize(dplyr::across(c(w), list(N = length, M = mean, SE = se, min = min, max = max))) %>%
#         dplyr::mutate(Parameters = 1, .before = 1),
#     dat.moral.numbers.fit.by.subj.2param %>%
#         dplyr::ungroup() %>%
#         filter_vivacity_exps() %>%
#         dplyr::filter(is.finite(w)) %>%
#         dplyr::group_by(experimentID, blocks, decisionType, vivacity) %>%
#         dplyr::summarize(dplyr::across(c(w, a), list(N = length, M = mean, SE = se, min = min, max = max))) %>%
#         dplyr::mutate(Parameters = 2, .before = 1)
# ) %>%
#     dplyr::rename(N = w_N) %>%
#     dplyr::select(-a_N) %>%
#     format_exp_info() %>%
#     dplyr::arrange(Parameters, experimentID)
# 
# # For long table, pack_rows must come last
# # See https://github.com/haozhu233/kableExtra/issues/476
# dat.moral.numbers.fit.by.subj.m %>%
#     dplyr::select(-Parameters) %>%
#     dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ stringr::str_replace(.x, "_", "\\_"))) %>%
#     knitr::kable(
#         # longtable = TRUE,
#         booktabs = TRUE,
#         escape = FALSE,
#         # linesep = c(rep("", 8), "\\addlinespace"),
#         caption = "Average of individual fits. PUT THIS IN THE APPENDIX AND ARGUE THAT WE DO NOT USE THOSE AND USE BOOTSTRAP FITS INSTEAD BECAUSE OF MANY SUBJECTS WHERE THE FIT DID NOT WORK"
#     ) %>%
#     kableExtra::kable_styling(latex_options = c(
#         "striped",
#         "scale_down",
#         "hold_position",
#         "repeat_header"
#     )) %>%
#     kableExtra::kable_classic() %>%
#     # See https://github.com/haozhu233/kableExtra/issues/476
#     kableExtra::pack_rows(index = dat.moral.numbers.fit.by.subj.m %>%
#                               dplyr::mutate(
#                                   Parameters =
#                                       stringr::str_c(Parameters, " parameters")
#                               ) %>%
#                               make.pack.index(Parameters))


## ----weber-ratio-moral-hist-plot, eval=FALSE, fig.cap="Histogram of individual $w$ parameters for moral decisions across Experiments 1 and 2. The $w$ parameter quantifies the precision of the numerical representation: lower values reflect more precise representations and thus greater sensitivity to the beneficiary:victim ratio. The histogram is truncated at $w = 3$ to improve readability."----
# plot.moral.numbers.hist.weber <- dat.moral.numbers.fit.by.subj.1param %>%
#     dplyr::filter(dplyr::case_when(
#         stringr::str_starts(experimentID, "exp1(b)*$") ~ TRUE,
#         stringr::str_starts(experimentID, "exp4") & decisionType == "moral" ~ TRUE,
#         TRUE ~ FALSE
#     )) %>%
#     format_exp_info() %>%
#     dplyr::mutate(experimentID = stringr::str_remove(experimentID, "\\D*$")) %>%
#     ggplot2::ggplot(ggplot2::aes(x = w)) +
#     ggplot2::geom_histogram(ggplot2::aes(y = ..density..), color = "#0000AA", fill = "#0000AA") +
#     ggplot2::geom_density(alpha = .8, fill = "#6666FF") +
#     #    theme_black (base_size = 16)+
#     ggplot2::labs(
#         x = latex2exp::TeX("Individual $w$ parameters"),
#         y = "Density"
#     ) +
#     ggplot2::xlim(0, 3) +
#     ggplot2::facet_wrap(. ~ experimentID)
# 
# plot.moral.numbers.hist.weber


## ----augment-individual-fits-with-ratings-1param-------------------------------------------------
dat.moral.numbers.fit.by.subj.1param.with.ratings <- dplyr::left_join(
    dat.moral.numbers.fit.by.subj.1param %>%
        # Get w
        dplyr::filter((experimentID %in%
                           dat.moral.numbers.exp.correspondance$experimentID.data) |
                          stringr::str_ends(experimentID, "vivacityManipulationWorking")) %>%
        dplyr::filter(blocks == "all"),
    dat.moral.numbers %>%
        dplyr::filter(question == "acceptability") %>%
        dplyr::group_by(experimentID, ResponseId, decisionType, vivacity) %>%
        dplyr::summarize(
            rating.raw = mean(rating.raw),
            rating.bin = mean(rating.bin),
            .groups = "drop"
        ) %>%
        dplyr::arrange(rating.raw) %>%
        dplyr::mutate(blocks = "all"),
    by = c("experimentID", "blocks", "ResponseId", "decisionType", "vivacity")
) %>%
    dplyr::ungroup() %>%
    dplyr::filter(is.finite(w) & is.finite(rating.raw))


## ----correlation-w-ratings, fig.cap="Spearman correlations between individual $w$ parameter estimates and average acceptability ratings across experiments. Higher $w$ values reflect less precise numerical representations and lower sensitivity to the beneficiary:victim ratio, predicting lower acceptability of the high-ratio option. All correlations were highly significant."----
dat.moral.numbers.fit.by.subj.1param.with.ratings %>%
    dplyr::filter(experimentID %in% c("exp1", "exp10", "exp10.vivacityManipulationWorking", "exp11", "exp11.vivacityManipulationWorking", "exp4", "exp8", "exp8.vivacityManipulationWorking")) %>%
    format_exp_info() %>%
    dplyr::group_by(experimentID, decisionType, vivacity) %>%
    tidyr::drop_na(w, rating.raw) %>%
    rstatix::cor_test(w, rating.raw, method = "spearman") %>%
    dplyr::mutate(cond = dplyr::case_when(
        !is.na(decisionType) ~ as.character(decisionType),
        !is.na(vivacity) ~ as.character(vivacity),
        TRUE ~ NA_character_
    )) %>%
    ggplot2::ggplot(ggplot2::aes(x = experimentID, y = cor, fill = cond)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8)) +
    ggplot2::geom_text(
        ggplot2::aes(label = cond),
        position = ggplot2::position_dodge(width = 0.8),
        hjust = -0.1, color = "black", size = 3,
        na.rm = TRUE # This will suppress NAs from being printed
    ) +
    ggplot2::scale_fill_manual(
        values = c(
            "vivid" = "gray40",
            "neutral" = "gray70",
            "moral" = "gray40",
            "economic" = "gray70",
            "NA" = "gray55"
        )
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme(
        legend.position = "none"
    ) +
    ggplot2::labs(
        y = TeX("Correlation between $w$ and (raw) acceptability ratings."),
        x = NULL
    )


## ----augment-individual-fits-with-ratings-2param-------------------------------------------------
dat.moral.numbers.fit.by.subj.2param.with.ratings <-
    dplyr::left_join(
        dat.moral.numbers.fit.by.subj.2param %>%
            # Get w and alpha
            dplyr::filter(
                !is.na(vivacity),
                blocks == "all",
                (experimentID %in%
                     dat.moral.numbers.exp.correspondance$experimentID.data) |
                    stringr::str_ends(experimentID, "vivacityManipulationWorking")
            ),
        dat.moral.numbers %>%
            dplyr::filter(
                !is.na(vivacity),
                (experimentID %in%
                     dat.moral.numbers.exp.correspondance$experimentID.data) |
                    stringr::str_ends(experimentID, "vivacityManipulationWorking")
            ) %>%
            dplyr::filter(question == "acceptability") %>%
            dplyr::group_by(experimentID, ResponseId, decisionType, vivacity) %>%
            dplyr::summarize(
                rating.raw = mean(rating.raw),
                rating.bin = mean(rating.bin),
                .groups = "drop"
            ) %>%
            dplyr::arrange(rating.raw) %>%
            dplyr::mutate(blocks = "all"),
        by = c("experimentID", "blocks", "ResponseId", "decisionType", "vivacity")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::filter(blocks == "all") %>%
    dplyr::select(-c(blocks, decisionType)) %>%
    tidyr::pivot_wider(
        id_cols = c("ResponseId", "experimentID"),
        names_from = "vivacity",
        values_from = c("w", "a", "rating.raw", "rating.bin")
    ) %>%
    tidyr::drop_na() %>%
    dplyr::mutate(rr = rating.bin_vivid / rating.bin_neutral)


## ----correlation-a-ratings, fig.cap="Spearman correlations between individual $\\alpha$ parameter estimates and (left) average acceptability ratings in the vivid condition and (right) the relative risk of acceptability ($\\RR = P_{acceptable, vivid} / P_{acceptable, neutral}$) across experiments with vividness manipulations. Higher $\\alpha$ values reflect greater attentional weight on the victims, predicting lower acceptability in the vivid condition and a lower relative risk. All correlations were highly significant."----
dat.moral.numbers.fit.by.subj.2param.with.ratings %>%
    filter_vivacity_exps() %>%
    format_exp_info() %>%
    dplyr::group_by(experimentID) %>%
    rstatix::cor_test(a_vivid, rating.raw_vivid, rr, method = "spearman") %>%
    dplyr::filter(
        !(
            (var1 == "rating.raw_vivid" & var2 == "rr") |
                (var2 == "rating.raw_vivid" & var1 == "rr")
        )
    ) %>%
    dplyr::group_by(experimentID) %>%
    make_cor_test_triangular() %>%
    dplyr::mutate(
        dplyr::across(
            dplyr::matches("^var[12]$"),
            \(x) {
                x %>%
                    stringr::str_replace("a_vivid", 'alpha*" (vivid)"') %>%
                    stringr::str_replace("rating.raw_vivid", '"Rating (vivid)"') %>%
                    stringr::str_replace("rr", '"RR"')
            }
        )
    ) %>%
    tidyr::unite(var_pair, var1, var2, sep = '*" vs. "*') %>%
    ggplot2::ggplot(ggplot2::aes(x = experimentID, y = cor)) +
    ggplot2::geom_col(position = ggplot2::position_dodge(width = 0.8)) +
    ggplot2::scale_fill_manual(
        values = c(
            "vivid" = "gray40",
            "neutral" = "gray70",
            "moral" = "gray40",
            "economic" = "gray70",
            "NA" = "gray55"
        )
    ) +
    ggplot2::coord_flip() +
    ggplot2::theme(
        legend.position = "none"
    ) +
    ggplot2::labs(
        y = TeX("Correlation between $\\alpha$ and other values."),
        x = NULL
    ) +
    ggplot2::facet_wrap(. ~ var_pair, labeller = ggplot2::label_parsed)


## ----fits-individual-with-forced-w-calculate-----------------------------------------------------
# Add individual Weber ratios in the neutral condition when both blocks are considered
# When considering only the first block, take the median as the first block is a between-subject
#    contrast, where no fits from the other condition are available.
dat.moral.numbers.fit.by.subj.forced.w <-
    # All blocks - use individual fits
    dplyr::left_join(
        dat.moral.numbers.for.bootstrap.fit %>%
            dplyr::ungroup() %>%
            # can be exp8 or exp8.vivacityManipulationWorking
            filter_vivacity_exps() %>%
            dplyr::filter(blocks == "all"),
        dat.moral.numbers.fit.by.subj.1param %>%
            dplyr::ungroup() %>%
            filter_vivacity_exps() %>%
            dplyr::filter(vivacity == "neutral") %>%
            dplyr::filter(blocks == "all") %>%
            dplyr::select(experimentID, ResponseId, blocks, w) %>%
            dplyr::rename(w.neutral = w),
        by = c("experimentID", "ResponseId", "blocks")
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(experimentID, blocks, ResponseId, vivacity, n.saved, n.victims, rating.bin, w.neutral) %>%
    na.omit() %>%
    dplyr::group_by(experimentID, blocks, ResponseId, vivacity) %>%
    tidyr::nest() %>%
    dplyr::mutate(tmp.fit = purrr::map(data, ~ get.weber.ratio(.x, fit.a = TRUE, fit.w = FALSE, w.chosen = unique(.x$w.neutral)))) %>%
    tidyr::unnest(tmp.fit) %>%
    dplyr::select(-data) %>%
    dplyr::ungroup()


## ----exp8-fits-individual-with-forced-w-print, eval = FALSE--------------------------------------
# dat.moral.numbers.fit.by.subj.forced.w.summary <- dat.moral.numbers.fit.by.subj.forced.w %>%
#     na.omit() %>%
#     dplyr::group_by(experimentID, blocks, vivacity) %>%
#     dplyr::summarize(
#         N = dplyr::n(),
#         M = mean(a),
#         SE = se(a),
#         p.wilcox = wilcox.p(a, mu = 1)
#     ) %>%
#     dplyr::ungroup()
# 
# dplyr::bind_rows(
#     dat.moral.numbers.fit.by.subj.forced.w.summary %>%
#         dplyr::rename(N1 = N) %>%
#         dplyr::mutate(test.type = "Test of the $\\alpha$ parameter against the chance level of 1", .before = 1),
#     dat.moral.numbers.fit.by.subj.forced.w %>%
#         dplyr::filter(blocks == "all") %>%
#         dplyr::arrange(experimentID, blocks, vivacity, ResponseId) %>%
#         dplyr::group_by(experimentID, blocks) %>%
#         rstatix::wilcox_test(a ~ vivacity, paired = TRUE) %>%
#         dplyr::select(experimentID, blocks, n1, n2, p) %>%
#         dplyr::rename(N1 = n1, N2 = n2, p.wilcox = p) %>%
#         dplyr::mutate(test.type = "Test of the $\\alpha$ parameter across the vividness conditions", .before = 1)
# ) %>%
#     dplyr::relocate(N2, .after = N1) %>%
#     format_exp_info() %>%
#     dplyr::arrange(desc(test.type), experimentID, blocks, vivacity) %>%
#     dplyr::select(-blocks) %>%
#     kable.packed(
#         "test.type",
#         caption = "By subject estimates of the $alpha$ paramenter in Experiment 8, where the w parameter was set to a fit from the one-parameter model in the neutral condition. When only the first block was considered (and thus only one block per participant), the w parameter was set to the median of the w parameter estimates in the neutral condition in the first block.",
#         # col.names = c(),
#         booktabs = TRUE,
#         longtable = FALSE,
#         escape = FALSE
#     ) %>%
#     kableExtra::kable_styling(latex_options = c(
#         "striped",
#         "scale_down",
#         "hold_position",
#         "repeat_header"
#     )) %>%
#     kableExtra::kable_classic_2()


## ----fits-individual-with-forced-w-plot, fig.cap = "Individual fits of the $\\alpha$ (attentional weight) parameter in Experiments 4 to 6, estimated with $w$ fixed to the bootstrap estimate from the neutral condition. Higher $\\alpha$ values indicate greater attentional weight placed on the victims relative to the beneficiaries. Each violin shows the distribution of individual estimates; the vividness manipulation consistently shifts $\\alpha$ upward in the vivid condition.", fig.height = 10----
dat.moral.numbers.fit.by.subj.forced.w %>%
    format_exp_info() %>%
    # We only have blocks == "all" anyhow
    dplyr::filter(blocks == "all") %>%
    na.omit() %>%
    ggplot2::ggplot(ggplot2::aes(x = vivacity, y = a)) %>%
    violin_plot_template() +
    ggplot2::scale_y_log10() +
    ggplot2::labs(y = TeX("$\\alpha$")) +
    ggplot2::facet_wrap(experimentID ~ ., ncol = 2)




## ----exp5-fit-bootstrap--------------------------------------------------------------------------
l.exp5.Z.vivid <- dat.moral.numbers.bootstrap.2param.summary %>%
    dplyr::filter(experimentID == "exp5", blocks == "all") %>%
    dplyr::ungroup() %>%
    make.Z.value.against.control(group = vivacity, controlCond = "neutral")

l.exp5b.Z.vivid <- dat.moral.numbers.bootstrap.2param.summary %>%
    dplyr::filter(experimentID == "exp5b", blocks == "all") %>%
    dplyr::ungroup() %>%
    make.Z.value.against.control(group = vivacity, controlCond = "neutral")

l.exp5ab.Z.vivid <- dat.moral.numbers.bootstrap.2param.summary %>%
    dplyr::filter(experimentID == "exp5ab", blocks == "first") %>%
    dplyr::ungroup() %>%
    make.Z.value.against.control(group = vivacity, controlCond = "neutral")


## ----exp5ab6-fit-plot, fig.cap = "Fit to a psychometric function for between-subject averages for Exp 5 (both blocks) and Exp 5b combined (1 block onlye). (Left) and Experiment 6 (Right, both blocks)"----
list(
    dplyr::bind_rows(
        dat.moral.numbers %>%
            dplyr::filter(experimentID == "exp5"),
        dat.moral.numbers %>%
            dplyr::filter(experimentID == "exp5b")
    ) %>%
        dplyr::mutate(experimentID = stringr::str_replace(experimentID, "exp5.*$", "exp5ab")) %>%
        # dplyr::filter(first.scenario.type == vivacity) %>%
        create.fit.plot(
            dat.fit = get_dat_fit_for_plot(experimentID == "exp5ab", blocks == "all",
                                           fit_type = FIT_FOR_FIGURE, n_fit_params = 2
            ),
            group = vivacity
        ),
    dat.moral.numbers %>%
        dplyr::filter(experimentID == "exp6") %>%
        create.fit.plot(
            dat.fit = get_dat_fit_for_plot(experimentID == "exp6", blocks == "all",
                                           fit_type = FIT_FOR_FIGURE, n_fit_params = 2
            ),
            group = vivacity
        )
) %>%
    ggpubr::ggarrange(
        plotlist = .,
        ncol = 2, nrow = 1, labels = "auto",
        common.legend = TRUE
    )


## ----exp6-fit-bootstrap--------------------------------------------------------------------------
# Z values for the w and a parameters for the vivid version with respect to the neutral version

l.exp6.Z.vivid <- dat.moral.numbers.bootstrap.2param.summary %>%
    dplyr::filter(experimentID == "exp6", blocks == "all") %>%
    dplyr::ungroup() %>%
    make.Z.value.against.control(group = vivacity, controlCond = "neutral")


## ----exp5ab6-bootstrap-fits-print----------------------------------------------------------------
dplyr::bind_rows(
    # Experiment F5: combined exp5 + exp5b, first block only
    dat.moral.numbers.bootstrap.2param.summary %>%
        dplyr::filter(experimentID == "exp5ab", blocks == "first") %>%
        dplyr::ungroup() %>%
        add_z_relative_to_baseline(vivacity) %>%
        dplyr::mutate(experimentID = "F5"),
    # Experiment F6: both blocks
    dat.moral.numbers.bootstrap.2param.summary %>%
        dplyr::filter(experimentID == "exp6", blocks == "all") %>%
        dplyr::ungroup() %>%
        add_z_relative_to_baseline(vivacity) %>%
        dplyr::mutate(experimentID = "F6")
) %>%
    dplyr::mutate(cond = vivacity) %>%
    dplyr::select(
        experimentID, cond,
        dplyr::ends_with("_N"),
        dplyr::ends_with("_M"),
        dplyr::ends_with("_mc_error"),
        dplyr::matches("_pi\\."),
        dplyr::matches("_Z(_p)*$")
    ) %>%
    dplyr::select(-dplyr::any_of("a_N")) %>%
    dplyr::mutate(dplyr::across(dplyr::ends_with("_Z_p"), fix_zero_p_values)) %>%
    knitr::kable(
        caption = "Two-parameter bootstrap fits for Experiments F5 (first block only) and F6 (both blocks). Bootstrap mean, MC error, 95\\% percentile interval, Z score vs.\\ 1, and associated $p$ value are shown for both the $w$ and $\\alpha$ parameters.",
        col.names = c(
            "Experiment", "Condition", "N",
            rep(c(" ", " ", "lower", "upper", " ", " "), 2)
        ),
        booktabs = TRUE,
        longtable = FALSE,
        escape = FALSE
    ) %>%
    kableExtra::kable_styling(latex_options = c(
        "striped",
        "scale_down",
        "hold_position",
        "repeat_header"
    )) %>%
    kableExtra::add_header_above(c(" " = 3, "M" = 1, "MC Error" = 1, "95% PI" = 2, "Z" = 1, "p" = 1, "M" = 1, "MC Error" = 1, "95% PI" = 2, "Z" = 1, "p" = 1), bold = FALSE) %>%
    kableExtra::add_header_above(c(" " = 3, "$w$" = 6, "$\\\\alpha$" = 6), bold = FALSE, align = "c", escape = FALSE) %>%
    kableExtra::kable_classic_2()


## ----exp7-check-attention-check------------------------------------------------------------------
dat.moral.numbers.exp7.failed.attention.check <- dat.moral.numbers %>%
    dplyr::filter(experimentID == "exp7") %>%
    dplyr::select(experimentID, ResponseId, passed.attention.check) %>%
    dplyr::distinct() %>%
    dplyr::filter(!passed.attention.check)

if (nrow(dat.moral.numbers.exp7.failed.attention.check) > 0) {
    dat.moral.numbers <- dplyr::anti_join(
        dat.moral.numbers,
        dat.moral.numbers.exp7.failed.attention.check,
        by = c("experimentID", "ResponseId")
    )
}


## ----exp7-calculate-severity-differences---------------------------------------------------------
# Difference between severity of victim and saved deaths collapsed across scenarios
dat.moral.numbers.exp7.severity.d.m.by.subj <- dat.moral.numbers %>%
    dplyr::filter(experimentID == "exp7") %>%
    dplyr::filter(stringr::str_detect(question, "severity")) %>%
    tidyr::pivot_wider(
        id_cols = c(experimentID, first.scenario.type, ResponseId, scenarioID, vivacity),
        names_from = question,
        values_from = rating.raw
    ) %>%
    dplyr::mutate(d_severity = make_diff_score(severity_victims, severity_saved)) %>%
    dplyr::group_by(experimentID, first.scenario.type, ResponseId, vivacity) %>%
    dplyr::summarize(d_severity = mean(d_severity))

# Difference between severity of victim and saved deaths separately for each scenario
dat.moral.numbers.exp7.severity.d.m.by.scenarios <- dat.moral.numbers %>%
    dplyr::filter(experimentID == "exp7") %>%
    dplyr::filter(stringr::str_detect(question, "severity")) %>%
    tidyr::pivot_wider(
        id_cols = c(experimentID, first.scenario.type, ResponseId, scenarioID, vivacity),
        names_from = question,
        values_from = rating.raw
    ) %>%
    dplyr::mutate(d_severity = make_diff_score(severity_victims, severity_saved)) %>%
    dplyr::group_by(experimentID, scenarioID, vivacity) %>%
    dplyr::summarize(d_severity = mean(d_severity))


## ----exp7-severity-plot-by-subjects-scenarios-raw, fig.cap = "Horrificness ratings in Experiment F7 for the deaths of the victims and the saved, respectively. (a) Averages by participants. (b) Averages by scenarios."----
ggpubr::ggarrange(
    dat.moral.numbers.exp7.severity.d.m.by.subj %>%
        #format_exp_info(wrap = TRUE) %>%
        ggplot2::ggplot(ggplot2::aes(x = vivacity, y = d_severity)) %>%
        violin_plot_template(yintercept = 0) +
        ggplot2::scale_y_continuous(TeX("Horrificness $\\frac{Victims - Saved}{Victims + Saved}$")) + # , limits = 0:1) +
        ggplot2::labs(
            x = "Vivacity",
            title = "By participants"
        ),
    dat.moral.numbers.exp7.severity.d.m.by.scenarios %>%
        #format_exp_info(wrap = TRUE) %>%
        # dplyr::mutate(experimentID = stringr::str_to_sentence(experimentID)) %>%
        # dplyr::mutate(experimentID = stringr::str_replace(experimentID, "victims", "harm")) %>%
        # dplyr::mutate(vivacity = stringr::str_to_title(vivacity)) %>%
        ggplot2::ggplot(ggplot2::aes(x = vivacity, y = d_severity)) %>%
        violin_plot_template(yintercept = 0) +
        ggplot2::scale_y_continuous(TeX("Horrificness $\\frac{Victims - Saved}{Victims + Saved}$")) + # , limits = 0:1) +
        ggplot2::labs(
            x = "Vivacity",
            title = "By scenarios"
        ) +
        ggplot2::geom_label(ggplot2::aes(label = scenarioID)),
    ncol = 2, nrow = 1, labels = "auto",
    common.legend = TRUE
)


## ----exp7-calculate-severity-proportion-of-positive-differences-per-subject----------------------
# Proportion of scenarios where the severity of victim deaths is greater than that of saved deaths separately
# for vivid scenarios only
dat.moral.numbers.exp7.severity.p.victims.greater.saved <- dat.moral.numbers %>%
    #dplyr::filter(experimentID == "exp7") %>%
    dplyr::filter(vivacity == "vivid") %>%
    dplyr::filter(stringr::str_detect(question, "severity")) %>%
    tidyr::pivot_wider(
        id_cols = c(experimentID, first.scenario.type, ResponseId, scenarioID),
        names_from = c(question),
        values_from = rating.raw
    ) %>%
    dplyr::mutate(d_severity_sign = factor(sign(severity_victims - severity_saved))) %>%
    dplyr::count(experimentID, first.scenario.type, ResponseId, d_severity_sign, .drop = FALSE) %>%
    dplyr::group_by(experimentID, first.scenario.type, ResponseId) %>%
    dplyr::mutate(freq = n / sum(n))


## ----identify-asymptote-define-function, include = FALSE-----------------------------------------
# Function moved to helper_functions/moral_numbers_asymptote.R


## ----identify-asymptote-define-conditions--------------------------------------------------------
dat_cond_find_asympt <- expand.grid(
    search.strategy = c("largest", "smallest"),
    test = c("wilcoxon", "mcnemar"),
    experimentID = NA,
    blocks = c("all", "first"),
    stringsAsFactors = FALSE
)



## ----identify-asymptote--------------------------------------------------------------------------
dat.moral.numbers.critical.ratios <-
    add_experimentID_to_cond_find_asympt(
        l_exp_with_single_severity_question_augmented,
        dat_cond_find_asympt
    ) %>%
    dplyr::mutate(
        critical.ratio =
            purrr::pmap_dbl(
                list(
                    search.strategy = search.strategy,
                    test = test,
                    experimentID = experimentID,
                    blocks = blocks
                ),
                ~ dat.moral.numbers.for.bootstrap.fit %>%
                    dplyr::filter(
                        experimentID == ..3,
                        blocks == ..4,
                        question == "acceptability",
                        vivacity == "vivid"
                    ) %>%
                    get.ratio.before.asymptote(search.strategy = ..1, test = ..2, verbose = !RECALCULATE_EVERYTHING_APP)
            )
    )


## ----identify-asymptote-print--------------------------------------------------------------------
dat.moral.numbers.critical.ratios %>%
    format_exp_info() %>%
    dplyr::arrange(experimentID, blocks, test, search.strategy) %>%
    dplyr::relocate(experimentID, .before = 1) %>%
    kable.packed(
        "experimentID",
        caption = "Asymptotes in Experiments 4 and 5. See main text for an explanation of the search strategies",
        col.names = (names)(.)[-1] %>%
            stringr::str_replace_all("\\.", " ") %>%
            stringr::str_to_sentence(),
        linesep = "",
        booktabs = TRUE,
        longtable = TRUE,
        escape = FALSE
    ) %>%
    kableExtra::kable_styling(latex_options = c(
        "striped",
        "scale_down",
        "hold_position"
    )) %>%
    kableExtra::kable_classic_2()


## ----identify-asymptote-define-functions-bootstrap1, include = FALSE-----------------------------
# Function moved to helper_functions/moral_numbers_asymptote.R


## ----identify-asymptote-define-functions-bootstrap2, include = FALSE-----------------------------
# Function moved to helper_functions/moral_numbers_asymptote.R


## ----exp8-bootstrap-asymptote-calculate----------------------------------------------------------
if (RECALCULATE_EVERYTHING_APP) {
    dat.moral.numbers.for.bootstrap.fit.asymptote.fits.exp8 <- run_bootstrap_fit_for_asymptote(
        dat = dat.moral.numbers.for.bootstrap.fit,
        v.exps = c("exp8", "exp8.vivacityManipulationWorking"),
        dat_cond = dat_cond_find_asympt
    )
    
    
    save(
        dat.moral.numbers.for.bootstrap.fit.asymptote.fits.exp8,
        file = here::here(
            OUTPUT_DIR,
            "bootstrap.asymptote.exp8.RData"
        )
    )
    
    gc() # Clean up memory just in case
} else {
    # Just load the data
    load(here::here(
        OUTPUT_DIR,
        "bootstrap.asymptote.exp8.RData"
    ))
}


## ----exp10-bootstrap-asymptote-calculate---------------------------------------------------------
if (RECALCULATE_EVERYTHING_APP) {
    dat.moral.numbers.for.bootstrap.fit.asymptote.fits.exp10 <- run_bootstrap_fit_for_asymptote(
        dat = dat.moral.numbers.for.bootstrap.fit,
        v.exps = c("exp10", "exp10.vivacityManipulationWorking"),
        dat_cond = dat_cond_find_asympt
    )
    
    save(
        dat.moral.numbers.for.bootstrap.fit.asymptote.fits.exp10,
        file = here::here(
            OUTPUT_DIR,
            "bootstrap.asymptote.exp10.RData"
        )
    )
    
    gc() # Clean up memory just in case
} else {
    # Just load the data
    load(here::here(
        OUTPUT_DIR,
        "bootstrap.asymptote.exp10.RData"
    ))
}


## ----exp8-10-bootstrap-asymptote-print, eval = FALSE---------------------------------------------
# dplyr::bind_rows(
#     dat.moral.numbers.for.bootstrap.fit.asymptote.fits.exp8,
#     dat.moral.numbers.for.bootstrap.fit.asymptote.fits.exp10
# ) %>%
#     dplyr::group_by(experimentID, blocks, test, search.strategy) %>%
#     dplyr::summarise(
#         N = sum(!is.na(critical.ratio)),
#         M = mean(critical.ratio, na.rm = TRUE),
#         SE = se(critical.ratio, na.rm = TRUE),
#         Range = paste0(range(critical.ratio, na.rm = TRUE), collapse = " - "),
#         .groups = "drop"
#     ) %>%
#     format_exp_info() %>%
#     dplyr::arrange(experimentID, blocks, search.strategy, test) %>%
#     kable.packed(
#         "experimentID",
#         caption = "Bootstrap fits for the critical ratios at the transistion between the pre-asymptotic and the asymptotic range",
#         # col.names = en_math_col_names(., 1),
#         booktabs = TRUE,
#         longtable = TRUE
#     ) %>%
#     kableExtra::kable_styling(latex_options = c(
#         "striped",
#         "scale_down",
#         "hold_position",
#         "repeat_header"
#     )) %>%
#     kableExtra::kable_classic_2()


## ----exp9a-print-number-conds, eval = FALSE------------------------------------------------------
# # There is also one big table somewhere, delete this in the paper
# dat.number.conds %>%
#     dplyr::ungroup() %>%
#     dplyr::filter(experimentID == "Experiment 9a") %>%
#     tidyr::unite("experimentID", experimentID, Group, sep = " - ", remove = TRUE, na.rm = TRUE) %>%
#     dplyr::group_by(experimentID) %>%
#     dplyr::arrange(ratio, n_victims, ratio2, n_victims2, .by_group = TRUE) %>%
#     dplyr::ungroup() %>%
#     dplyr::rename_with(~ stringr::str_remove(.x, "n_")) %>%
#     dplyr::select(dplyr::matches("[^2]$", perl = TRUE)) %>%
#     kable.packed("experimentID",
#                  caption = "Number conditions in Experiment 9a",
#                  booktabs = TRUE,
#                  longtable = TRUE,
#                  digits = 3
#     ) %>%
#     kableExtra::add_header_above(c("Number of" = 3, " " = 1), bold = TRUE) %>%
#     kableExtra::kable_classic_2()


## ----exp9a-severity-plot-by-subjects-scenarios-raw, fig.cap = "Relative horrificness ratings in Experiment 9a for the deaths of the victims and the saved, respectively. (a) Averages by participants. (b) Averages by scenarios.  (d) Differences between the vivid and the neutral condition by scenarios.", fig.height=8----
dplyr::bind_rows(
    dat.moral.numbers.severity.m.by.subj %>%
        dplyr::filter(experimentID == "exp9a") %>%
        dplyr::mutate(m.type = "By participants"),
    dat.moral.numbers.severity.m.by.scenarios %>%
        dplyr::filter(experimentID == "exp9a") %>%
        dplyr::mutate(m.type = "By scenarios")
) %>%
    format_exp_info(wrap = TRUE) %>%
    # dplyr::mutate(experimentID = stringr::str_to_sentence(experimentID)) %>%
    # dplyr::mutate(experimentID = str_replace(experimentID, "victims", "harm")) %>%
    # dplyr::mutate(vivacity = stringr::str_to_title(vivacity)) %>%
    ggplot2::ggplot(ggplot2::aes(x = m.type, y = rating)) %>%
    violin_plot_template(yintercept = 3.5) +
    ggplot2::scale_y_continuous("Relative Horrificness (Victim > Beneficiary)") + # , limits = 0:1) +
    ggplot2::labs(x = NULL) +
    ggplot2::geom_label(ggplot2::aes(label = scenarioID))


## ----exp9a-plot-bin-prepare----------------------------------------------------------------------
l.exp9a.model.comp.plots.bin.unfiltered <- dat.moral.numbers %>%
    dplyr::filter(experimentID == "exp9a") %>%
    dplyr::filter(question == "acceptability") %>%
    dplyr::mutate(vivacity = stringr::str_to_title(vivacity)) %>%
    create.predictor.comparison.plot(
        facet.var = NULL,
        dat.fit = get_dat_fit_for_plot(experimentID == "exp9a", blocks == "all",
                                       fit_type = FIT_FOR_FIGURE, n_fit_params = 1
        ),
        value.var = rating.bin,
        col.var = vivacity,
        ylab = "Acceptability",
        legend = "bottom",
        add.fit = TRUE,
        return.plot = FALSE,
        add.p.saved.to.list = TRUE
    )


l.exp9a.model.comp.plots.bin.filtered <- dat.moral.numbers %>%
    dplyr::filter(experimentID == "exp9a.vivacityManipulationWorking") %>%
    dplyr::filter(question == "acceptability") %>%
    dplyr::mutate(vivacity = stringr::str_to_title(vivacity)) %>%
    create.predictor.comparison.plot(
        facet.var = NULL,
        dat.fit = get_dat_fit_for_plot(experimentID == "exp9a.vivacityManipulationWorking", blocks == "all",
                                       fit_type = FIT_FOR_FIGURE, n_fit_params = 1
        ),
        value.var = rating.bin,
        col.var = vivacity,
        ylab = "Acceptability",
        legend = "bottom",
        add.fit = TRUE,
        return.plot = FALSE,
        add.p.saved.to.list = TRUE
    )


## ----exp9a-plot-raw-prepare----------------------------------------------------------------------
l.exp9a.model.comp.plots.raw.unfiltered <- dat.moral.numbers %>%
    dplyr::filter(experimentID == "exp9a") %>%
    # dplyr::filter(first.scenario.type == vivacity) %>%
    dplyr::filter(question == "acceptability") %>%
    create.predictor.comparison.plot(
        facet.var = NULL,
        value.var = rating.raw,
        col.var = vivacity,
        ylab = "Rating (raw)",
        legend = "bottom",
        add.fit = FALSE,
        return.plot = FALSE,
        add.p.saved.to.list = TRUE
    )


l.exp9a.model.comp.plots.raw.filtered <- dat.moral.numbers %>%
    dplyr::filter(experimentID == "exp9a.vivacityManipulationWorking") %>%
    # dplyr::filter(first.scenario.type == vivacity) %>%
    dplyr::filter(question == "acceptability") %>%
    create.predictor.comparison.plot(
        facet.var = NULL,
        value.var = rating.raw,
        col.var = vivacity,
        ylab = "Rating (raw)",
        legend = "bottom",
        add.fit = FALSE,
        return.plot = FALSE,
        add.p.saved.to.list = TRUE
    )


## ----exp9a-plot, fig.cap = "Results of Experiment 9a, where participants were filtered based on their responses on the severity question. Error bars represent 95\\% bootstrap confidence intervals. Model fits are based on on the average response of all participants. REPLACE WITH BOOTSTRAP FIT AND KEEP ONLY BINARY AND FILTERED DATA. (a) filter - binary, (b) unfiltered - binary, (c) filtered - raw, (d) unfiltered - raw", eval = TRUE, fig.height=9, fig.width=9----
ggpubr::ggarrange(
    l.exp9a.model.comp.plots.bin.filtered[[1]] +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust = 1)) +
        ggplot2::labs(title = "Filtered, binary") +
        ggplot2::scale_x_continuous(
            breaks = l.moral.numbers.ratios$exp9a,
            trans = "log2"
        ),
    # coord_trans(x = "log2"),
    
    l.exp9a.model.comp.plots.bin.unfiltered[[1]] +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust = 1)) +
        ggplot2::labs(title = "Unfiltered, binary") +
        ggplot2::scale_x_continuous(
            breaks = l.moral.numbers.ratios$exp9a,
            trans = "log2"
        ),
    # coord_trans(x = "log2"),
    
    l.exp9a.model.comp.plots.raw.filtered[[1]] +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust = 1)) +
        ggplot2::labs(title = "Filtered, raw") +
        ggplot2::scale_x_continuous(
            breaks = l.moral.numbers.ratios$exp9a,
            trans = "log2"
        ),
    # coord_trans(x = "log2"),
    
    
    l.exp9a.model.comp.plots.raw.unfiltered[[1]] +
        ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, vjust = 0.5, hjust = 1)) +
        ggplot2::labs(title = "Unfiltered, raw") +
        ggplot2::scale_x_continuous(
            breaks = l.moral.numbers.ratios$exp9a,
            trans = "log2"
        ),
    # coord_trans(x = "log2"),
    ncol = 2, nrow = 2, labels = "auto",
    legend = "bottom",
    common.legend = TRUE
) %>%
    ggpubr::annotate_figure(
        fig.lab = "Exp. 9a - plots as a function of ratio  filtered and unfiltered data",
        fig.lab.pos = "bottom.left",
        fig.lab.size = 11,
        fig.lab.face = "bold"
    )


## ----asymptotic-vs-preasymptotic-range-raw-calculate---------------------------------------------
# By ratio range (asymptotic vs pre-asymptotic)
dat.moral.numbers.asymptotic.vs.preasymptotic.raw.by.ratio.range <- dat.moral.numbers %>%
    dplyr::filter(
        stringr::str_detect(experimentID, stringr::str_c(l_exp_with_single_severity_question, collapse = "|")),
        question == "acceptability"
    ) %>%
    dplyr::mutate(
        asymptote.start = unname(v.asymptote.start[stringr::str_remove(
            experimentID,
            ".vivacityManipulationWorking"
        )]),
        ratio.range = factor(
            ifelse(ratio > asymptote.start,
                   "asymptote", "pre-asymptote"
            ),
            levels = c("pre-asymptote", "asymptote")
        )
    ) %>%
    dplyr::group_by(experimentID, ratio.range, vivacity, ResponseId) %>%
    dplyr::summarise(rating = mean(rating.raw)) %>%
    dplyr::ungroup() %>%
    tidyr::pivot_wider(
        id_cols = c(experimentID, ratio.range, ResponseId),
        names_from = vivacity,
        values_from = rating
    ) %>%
    dplyr::mutate(
        d.absolute = neutral - vivid,
        d.relative = 7 / 5 * (neutral - vivid) / (neutral + vivid)
    )

# By ratio


## ----exp8-asymptotic-vs-preasymptotic-range-raw-plot, fig.cap = "Differences for raw ratings between the vivid and neutral condition as a function of ratio range."----
dat.moral.numbers.asymptotic.vs.preasymptotic.raw.by.ratio.range %>%
    dplyr::filter(stringr::str_detect(experimentID, "exp8")) %>%
    format_exp_info() %>%
    tidyr::pivot_longer(dplyr::starts_with("d."),
                        names_to = "difference.type",
                        values_to = "d"
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = ratio.range, y = d)) %>%
    violin_plot_template(add.dot.plot = FALSE, yintercept = 0) +
    # Don't use ylim since it throws away data !
    ggplot2::coord_cartesian(ylim = c(0, .5)) +
    ggplot2::facet_wrap(experimentID ~ difference.type, scales = "free")


## ----asymptotic-vs-preasymptotic-range-raw-prepare-----------------------------------------------
dat.moral.numbers.asymptotic.vs.preasymptotic.raw.by.ratio.range.comparisons <- dplyr::bind_rows(
    # Compare individual difference scores to zero
    dat.moral.numbers.asymptotic.vs.preasymptotic.raw.by.ratio.range %>%
        dplyr::filter(stringr::str_detect(experimentID, "exp8|exp10")) %>%
        rstatix::df_nest_by(experimentID, ratio.range) %>%
        dplyr::mutate(tmp = purrr::map(
            data,
            ~ dplyr::bind_rows(
                dplyr::bind_cols(
                    rstatix::wilcox_test(.x,
                                         d.absolute ~ 1,
                                         mu = 0,
                                         detailed = FALSE
                    ),
                    data.frame(Median = median(.x$d.absolute), M = mean(.x$d.absolute), SD = sd(.x$d.absolute))
                ),
                dplyr::bind_cols(
                    rstatix::wilcox_test(.x,
                                         d.relative ~ 1,
                                         mu = 0,
                                         detailed = FALSE
                    ),
                    data.frame(Median = median(.x$d.relative), M = mean(.x$d.relative), SD = sd(.x$d.relative))
                )
            )
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(tmp) %>%
        dplyr::arrange(.y., ratio.range) %>%
        dplyr::rename(difference.type = .y.) %>%
        dplyr::rename(n1 = n) %>%
        dplyr::mutate(test.type = "Individual difference scores against change", .before = 1),
    
    
    # Compare the difference scores across ratio ranges
    dat.moral.numbers.asymptotic.vs.preasymptotic.raw.by.ratio.range %>%
        dplyr::filter(stringr::str_detect(experimentID, "exp8|exp10")) %>%
        tidyr::pivot_longer(c(d.absolute, d.relative),
                            names_to = "difference.type",
                            values_to = "d"
        ) %>%
        dplyr::arrange(difference.type, ratio.range, ResponseId) %>%
        rstatix::df_nest_by(experimentID, difference.type) %>%
        dplyr::mutate(tmp = purrr::map(
            data,
            ~ .x %>%
                rstatix::wilcox_test(d ~ ratio.range, paired = TRUE, detailed = FALSE)
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(tmp) %>%
        dplyr::select(-.y.) %>%
        dplyr::mutate(test.type = "Difference scores comparison across ratio ranges", .before = 1)
) %>%
    dplyr::select(-group1, -group2) %>%
    dplyr::relocate(n2, .after = n1) %>%
    dplyr::relocate(difference.type, .after = ratio.range) %>%
    dplyr::relocate(Median, M, SD, .before = p) %>%
    dplyr::arrange(desc(test.type), desc(experimentID), difference.type, ratio.range)


## ----exp10-asymptotic-vs-preasymptotic-range-raw-plot, fig.cap = "Differences for raw ratings between the vivid and neutral condition as a function of ratio range in Experiment 5."----
dat.moral.numbers.asymptotic.vs.preasymptotic.raw.by.ratio.range %>%
    dplyr::filter(stringr::str_detect(experimentID, "exp10")) %>%
    format_exp_info() %>%
    tidyr::pivot_longer(dplyr::starts_with("d."),
                        names_to = "difference.type",
                        values_to = "d"
    ) %>%
    ggplot2::ggplot(ggplot2::aes(x = ratio.range, y = d)) %>%
    violin_plot_template(add.dot.plot = FALSE, yintercept = 0) +
    # Don't use ylim since it throws away data !
    ggplot2::coord_cartesian(ylim = c(0, .5)) +
    ggplot2::facet_wrap(experimentID ~ difference.type, scales = "free")


## ----exp8-10-asymptotic-vs-preasymptotic-range-raw-plot, fig.cap = "Differences for raw ratings between the vivid and neutral condition as a function of ratio range in Experiments 4 and 5 combined."----
dat.moral.numbers.asymptotic.vs.preasymptotic.raw.by.ratio.range %>%
    dplyr::filter(stringr::str_detect(experimentID, "exp8|exp10")) %>%
    tidyr::pivot_longer(dplyr::starts_with("d."),
                        names_to = "difference.type",
                        values_to = "d"
    ) %>%
    # dplyr::group_by(experimentID, difference.type) %>%
    # dplyr::mutate(Z = abs(scale(d))) %>%
    # dplyr::filter(Z < 3) %>%
    dplyr::mutate(experimentID = stringr::str_replace(
        experimentID,
        "^exp\\d+", "exp8+10"
    )) %>%
    ggplot2::ggplot(ggplot2::aes(x = ratio.range, y = d)) %>%
    violin_plot_template(add.dot.plot = FALSE, yintercept = 0) +
    # Don't use ylim since it throws away data !
    ggplot2::coord_cartesian(ylim = c(0, .5)) +
    ggplot2::facet_wrap(experimentID ~ difference.type, scales = "free")


## ----exp8-10-asymptotic-vs-preasymptotic-range-raw-prepare---------------------------------------
dat.moral.numbers.exp8.10.asymptotic.vs.preasymptotic.raw.by.ratio.range.comparisons <- dplyr::bind_rows(
    # Compare individual difference scores to zero
    dat.moral.numbers.asymptotic.vs.preasymptotic.raw.by.ratio.range %>%
        dplyr::filter(stringr::str_detect(experimentID, "exp8|exp10")) %>%
        dplyr::mutate(experimentID = stringr::str_replace(experimentID, "^exp\\d+", "exp8+10")) %>%
        rstatix::df_nest_by(experimentID, ratio.range) %>%
        dplyr::mutate(tmp = purrr::map(
            data,
            ~ dplyr::bind_rows(
                dplyr::bind_cols(
                    rstatix::wilcox_test(.x,
                                         d.absolute ~ 1,
                                         mu = 0,
                                         detailed = FALSE
                    ),
                    data.frame(Median = median(.x$d.absolute), M = mean(.x$d.absolute), SD = sd(.x$d.absolute))
                ),
                dplyr::bind_cols(
                    rstatix::wilcox_test(.x,
                                         d.relative ~ 1,
                                         mu = 0,
                                         detailed = FALSE
                    ),
                    data.frame(Median = median(.x$d.relative), M = mean(.x$d.relative), SD = sd(.x$d.relative))
                )
            )
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(tmp) %>%
        dplyr::arrange(.y., ratio.range) %>%
        dplyr::rename(difference.type = .y.) %>%
        dplyr::rename(n1 = n) %>%
        dplyr::mutate(test.type = "Individual difference scores against change", .before = 1),
    
    
    # Compare the difference scores across ratio ranges
    dat.moral.numbers.asymptotic.vs.preasymptotic.raw.by.ratio.range %>%
        dplyr::filter(stringr::str_detect(experimentID, "exp8|exp10")) %>%
        dplyr::mutate(experimentID = stringr::str_replace(experimentID, "^exp\\d+", "exp8+10")) %>%
        tidyr::pivot_longer(c(d.absolute, d.relative),
                            names_to = "difference.type",
                            values_to = "d"
        ) %>%
        dplyr::ungroup() %>%
        dplyr::arrange(difference.type, ratio.range, ResponseId) %>%
        rstatix::df_nest_by(experimentID, difference.type) %>%
        dplyr::mutate(tmp = purrr::map(
            data,
            ~ .x %>%
                rstatix::wilcox_test(d ~ ratio.range, paired = TRUE, detailed = FALSE)
        )) %>%
        dplyr::select(-data) %>%
        tidyr::unnest(tmp) %>%
        dplyr::select(-.y.) %>%
        dplyr::mutate(test.type = "Difference scores comparison across ratio ranges", .before = 1)
) %>%
    dplyr::select(-group1, -group2) %>%
    dplyr::relocate(n2, .after = n1) %>%
    dplyr::relocate(difference.type, .after = ratio.range) %>%
    dplyr::relocate(Median, M, SD, .before = p) %>%
    dplyr::arrange(desc(test.type), desc(experimentID), difference.type, ratio.range)


## ----asymptotic-vs-preasymptotic-range-raw-print-------------------------------------------------
dplyr::bind_rows(
    dat.moral.numbers.asymptotic.vs.preasymptotic.raw.by.ratio.range.comparisons,
    dat.moral.numbers.exp8.10.asymptotic.vs.preasymptotic.raw.by.ratio.range.comparisons
) %>%
    format_exp_info() %>%
    fix_zero_p_values(p) %>% 
    dplyr::arrange(desc(test.type), experimentID, difference.type, ratio.range) %>%
    dplyr::rename_with(~ stringr::str_replace_all(.x, "\\.", " ")) %>%
    kable.packed(
        "test type",
        caption = "Wilcoxon tests of vivacity difference scores (vivid minus neutral acceptability) in Experiments 4 and 5. Top panel: one-sample tests against zero (i.e., whether vivacity affected acceptability). Bottom panel: paired tests comparing pre-asymptotic vs.\\ asymptotic ratio ranges (i.e., whether the vivacity effect was larger in the pre-asymptotic range). Rows labelled (filtered) are restricted to participants for whom the vivacity manipulation was successful.",
        linesep = "",
        booktabs = TRUE,
        longtable = FALSE,
        escape = FALSE
    ) %>%
    # kableExtra::row_spec(9:12, color = "red") %>%
    kableExtra::kable_styling(latex_options = c( # "striped",
        # "hold_position",
        "scale_down"
    )) %>%
    kableExtra::kable_classic_2()


## ----exp8-asymptotic-vs-preasymptotic-range-bin-glmm-calculate-----------------------------------
dat.moral.numbers.exp8.bin.lmer.both.ratio.ranges <- dat.moral.numbers %>%
    dplyr::filter(stringr::str_starts(experimentID, "exp8")) %>%
    dplyr::filter(question == "acceptability") %>%
    dplyr::mutate(ratio.range = factor(
        ifelse(ratio > unname(v.asymptote.start["exp8"]), "asymptote", "pre-asymptote"),
        levels = c("pre-asymptote", "asymptote")
    )) %>%
    dplyr::mutate(vivacity = factor(vivacity, levels = c("neutral", "vivid"))) %>%
    dplyr::group_by(experimentID) %>%
    dplyr::group_modify(~ lme4::glmer(
        rating.bin ~ ratio.range * vivacity +
            (1 | ResponseId) + (1 | scenarioID),
        control = glmerControl(optimizer = "bobyqa"),
        family = "binomial",
        data = .x
    ) %>%
        extract.results.from.binary.model())


dat.moral.numbers.exp8.bin.lmer.pre.asymptotic.ratio.range <- dat.moral.numbers %>%
    dplyr::filter(stringr::str_starts(experimentID, "exp8")) %>%
    dplyr::filter(question == "acceptability") %>%
    dplyr::mutate(ratio.range = factor(
        ifelse(ratio > unname(v.asymptote.start["exp8"]), "asymptote", "pre-asymptote"),
        levels = c("pre-asymptote", "asymptote")
    )) %>%
    dplyr::mutate(vivacity = factor(vivacity, levels = c("neutral", "vivid"))) %>%
    dplyr::filter(ratio.range != "asymptote") %>%
    dplyr::group_by(experimentID) %>%
    dplyr::group_modify(~ lme4::glmer(
        rating.bin ~ vivacity +
            (1 | ResponseId) + (1 | scenarioID),
        control = glmerControl(optimizer = "bobyqa"),
        family = "binomial",
        data = .x
    ) %>%
        extract.results.from.binary.model())

dat.moral.numbers.exp8.bin.lmer.asymptotic.ratio.range <- dat.moral.numbers %>%
    dplyr::filter(stringr::str_starts(experimentID, "exp8")) %>%
    dplyr::filter(question == "acceptability") %>%
    dplyr::mutate(ratio.range = factor(
        ifelse(ratio > unname(v.asymptote.start["exp8"]), "asymptote", "pre-asymptote"),
        levels = c("pre-asymptote", "asymptote")
    )) %>%
    dplyr::mutate(vivacity = factor(vivacity, levels = c("neutral", "vivid"))) %>%
    dplyr::filter(ratio.range == "asymptote") %>%
    dplyr::group_by(experimentID) %>%
    dplyr::group_modify(~ lme4::glmer(
        rating.bin ~ vivacity +
            (1 | ResponseId),
        control = glmerControl(optimizer = "bobyqa"),
        family = "binomial",
        data = .x
    ) %>%
        extract.results.from.binary.model())


## ----exp8-asymptotic-vs-preasymptotic-range-bin-glmm-print, eval = FALSE-------------------------
# dplyr::bind_rows(
#     dat.moral.numbers.exp8.bin.lmer.both.ratio.ranges %>%
#         dplyr::mutate(model = "Both ratio ranges", .before = 1),
#     dat.moral.numbers.exp8.bin.lmer.pre.asymptotic.ratio.range %>%
#         dplyr::mutate(model = "Pre-asymptotic range", .before = 1),
#     dat.moral.numbers.exp8.bin.lmer.asymptotic.ratio.range %>%
#         dplyr::mutate(model = "Asymptotic range", .before = 1)
# ) %>%
#     dplyr::select(-t_log, -p_log) %>%
#     rename.terms(term) %>%
#     dplyr::arrange(experimentID) %>%
#     tidyr::unite(experimentID.model, experimentID, model, sep = " - ") %>%
#     kable.packed(
#         "experimentID.model",
#         caption = "GLMM for binary data in Experiment 8 for filtered data. If didn't compute the unfiltered GLMMS since even the filtered once don't work.",
#         col.names = c("Effect", "Estimate", "SE", "CI", "Estimate", "SE", "CI", "t", "p"),
#         booktabs = TRUE,
#         longtable = FALSE,
#         escape = FALSE
#     ) %>%
#     kableExtra::add_header_above(c(" " = 1, "Log-odds space" = 3, "Odds-ratio space" = 3, " " = 2)) %>%
#     kableExtra::kable_styling(latex_options = c(
#         "striped",
#         "scale_down",
#         "hold_position",
#         "repeat_header"
#     )) %>%
#     kableExtra::kable_classic_2()


## ----exp10-asymptotic-vs-preasymptotic-range-bin-glmm-calculate----------------------------------
dat.moral.numbers.exp10.bin.lmer.both.ratio.ranges <- dat.moral.numbers %>%
    dplyr::filter(stringr::str_starts(experimentID, "exp10")) %>%
    dplyr::filter(question == "acceptability") %>%
    dplyr::mutate(ratio.range = factor(
        ifelse(ratio > unname(v.asymptote.start["exp10"]), "asymptote", "pre-asymptote"),
        levels = c("pre-asymptote", "asymptote")
    )) %>%
    dplyr::mutate(vivacity = factor(vivacity, levels = c("neutral", "vivid"))) %>%
    dplyr::group_by(experimentID) %>%
    dplyr::group_modify(~ lme4::glmer(
        rating.bin ~ ratio.range * vivacity +
            (1 | ResponseId) + (1 | scenarioID),
        control = glmerControl(optimizer = "bobyqa"),
        family = "binomial",
        data = .x
    ) %>%
        extract.results.from.binary.model())


dat.moral.numbers.exp10.bin.lmer.pre.asymptotic.ratio.range <- dat.moral.numbers %>%
    dplyr::filter(stringr::str_starts(experimentID, "exp10")) %>%
    dplyr::filter(question == "acceptability") %>%
    dplyr::mutate(ratio.range = factor(
        ifelse(ratio > unname(v.asymptote.start["exp10"]), "asymptote", "pre-asymptote"),
        levels = c("pre-asymptote", "asymptote")
    )) %>%
    dplyr::mutate(vivacity = factor(vivacity, levels = c("neutral", "vivid"))) %>%
    dplyr::filter(ratio.range != "asymptote") %>%
    dplyr::group_by(experimentID) %>%
    dplyr::group_modify(~ lme4::glmer(
        rating.bin ~ vivacity +
            (1 | ResponseId) + (1 | scenarioID),
        control = glmerControl(optimizer = "bobyqa"),
        family = "binomial",
        data = .x
    ) %>%
        extract.results.from.binary.model())

dat.moral.numbers.exp10.bin.lmer.asymptotic.ratio.range <- dat.moral.numbers %>%
    dplyr::filter(stringr::str_starts(experimentID, "exp10")) %>%
    dplyr::filter(question == "acceptability") %>%
    dplyr::mutate(ratio.range = factor(
        ifelse(ratio > unname(v.asymptote.start["exp10"]), "asymptote", "pre-asymptote"),
        levels = c("pre-asymptote", "asymptote")
    )) %>%
    dplyr::mutate(vivacity = factor(vivacity, levels = c("neutral", "vivid"))) %>%
    dplyr::filter(ratio.range == "asymptote") %>%
    dplyr::group_by(experimentID) %>%
    dplyr::group_modify(~ lme4::glmer(
        rating.bin ~ vivacity +
            (1 | ResponseId),
        control = glmerControl(optimizer = "bobyqa"),
        family = "binomial",
        data = .x
    ) %>%
        extract.results.from.binary.model())


## ----exp8-10-asymptotic-vs-preasymptotic-range-bin-glmm-calculate--------------------------------
dat.moral.numbers.exp8.10.bin.lmer.both.ratio.ranges <- dplyr::bind_rows(
    dat.moral.numbers %>%
        dplyr::filter(stringr::str_starts(experimentID, "exp8")) %>%
        dplyr::filter(question == "acceptability") %>%
        dplyr::mutate(ratio.range = factor(
            ifelse(ratio > unname(v.asymptote.start["exp8"]), "asymptote", "pre-asymptote"),
            levels = c("pre-asymptote", "asymptote")
        )),
    dat.moral.numbers %>%
        dplyr::filter(stringr::str_starts(experimentID, "exp10")) %>%
        dplyr::filter(question == "acceptability") %>%
        dplyr::mutate(ratio.range = factor(
            ifelse(ratio > unname(v.asymptote.start["exp10"]), "asymptote", "pre-asymptote"),
            levels = c("pre-asymptote", "asymptote")
        ))
) %>%
    dplyr::mutate(experimentID = stringr::str_replace(experimentID, "^exp\\d+", "exp8+10")) %>%
    dplyr::mutate(vivacity = factor(vivacity, levels = c("neutral", "vivid"))) %>%
    dplyr::group_by(experimentID) %>%
    dplyr::group_modify(~ lme4::glmer(
        rating.bin ~ ratio.range * vivacity +
            (1 | ResponseId) + (1 | scenarioID),
        control = glmerControl(optimizer = "bobyqa"),
        family = "binomial",
        data = .x
    ) %>%
        extract.results.from.binary.model())


dat.moral.numbers.exp8.10.bin.lmer.pre.asymptotic.ratio.range <- dplyr::bind_rows(
    dat.moral.numbers %>%
        dplyr::filter(stringr::str_starts(experimentID, "exp8")) %>%
        dplyr::filter(question == "acceptability") %>%
        dplyr::mutate(ratio.range = factor(
            ifelse(ratio > unname(v.asymptote.start["exp8"]), "asymptote", "pre-asymptote"),
            levels = c("pre-asymptote", "asymptote")
        )),
    dat.moral.numbers %>%
        dplyr::filter(stringr::str_starts(experimentID, "exp10")) %>%
        dplyr::filter(question == "acceptability") %>%
        dplyr::mutate(ratio.range = factor(
            ifelse(ratio > unname(v.asymptote.start["exp10"]), "asymptote", "pre-asymptote"),
            levels = c("pre-asymptote", "asymptote")
        ))
) %>%
    dplyr::mutate(experimentID = stringr::str_replace(experimentID, "^exp\\d+", "exp8+10")) %>%
    dplyr::mutate(vivacity = factor(vivacity, levels = c("neutral", "vivid"))) %>%
    dplyr::filter(ratio.range != "asymptote") %>%
    dplyr::group_by(experimentID) %>%
    dplyr::group_modify(~ lme4::glmer(
        rating.bin ~ vivacity +
            (1 | ResponseId) + (1 | scenarioID),
        control = glmerControl(optimizer = "bobyqa"),
        family = "binomial",
        data = .x
    ) %>%
        extract.results.from.binary.model())

dat.moral.numbers.exp8.10.bin.lmer.asymptotic.ratio.range <- dplyr::bind_rows(
    dat.moral.numbers %>%
        dplyr::filter(stringr::str_starts(experimentID, "exp8")) %>%
        dplyr::filter(question == "acceptability") %>%
        dplyr::mutate(ratio.range = factor(
            ifelse(ratio > unname(v.asymptote.start["exp8"]), "asymptote", "pre-asymptote"),
            levels = c("pre-asymptote", "asymptote")
        )),
    dat.moral.numbers %>%
        dplyr::filter(stringr::str_starts(experimentID, "exp10")) %>%
        dplyr::filter(question == "acceptability") %>%
        dplyr::mutate(ratio.range = factor(
            ifelse(ratio > unname(v.asymptote.start["exp10"]), "asymptote", "pre-asymptote"),
            levels = c("pre-asymptote", "asymptote")
        ))
) %>%
    dplyr::mutate(experimentID = stringr::str_replace(experimentID, "^exp\\d+", "exp8+10")) %>%
    dplyr::mutate(vivacity = factor(vivacity, levels = c("neutral", "vivid"))) %>%
    dplyr::filter(ratio.range == "asymptote") %>%
    dplyr::group_by(experimentID) %>%
    dplyr::group_modify(~ lme4::glmer(
        rating.bin ~ vivacity +
            (1 | ResponseId),
        control = glmerControl(optimizer = "bobyqa"),
        family = "binomial",
        data = .x
    ) %>%
        extract.results.from.binary.model())


## ----asymptotic-vs-preasymptotic-range-bin-glmm-print--------------------------------------------

dat.moral.numbers.asymptotic.vs.preasymptotic.raw.by.ratio.range.glmm.for.table <- 
    dplyr::bind_rows(
        dat.moral.numbers.exp8.bin.lmer.both.ratio.ranges %>%
            dplyr::mutate(model = "Both ratio ranges", .before = 1),
        dat.moral.numbers.exp8.bin.lmer.pre.asymptotic.ratio.range %>%
            dplyr::mutate(model = "Pre-asymptotic range", .before = 1),
        dat.moral.numbers.exp8.bin.lmer.asymptotic.ratio.range %>%
            dplyr::mutate(model = "Asymptotic range", .before = 1),
        dat.moral.numbers.exp10.bin.lmer.both.ratio.ranges %>%
            dplyr::mutate(model = "Both ratio ranges", .before = 1),
        dat.moral.numbers.exp10.bin.lmer.pre.asymptotic.ratio.range %>%
            dplyr::mutate(model = "Pre-asymptotic range", .before = 1),
        dat.moral.numbers.exp10.bin.lmer.asymptotic.ratio.range %>%
            dplyr::mutate(model = "Asymptotic range", .before = 1),
        dat.moral.numbers.exp8.10.bin.lmer.both.ratio.ranges %>%
            dplyr::mutate(model = "Both ratio ranges", .before = 1),
        dat.moral.numbers.exp8.10.bin.lmer.pre.asymptotic.ratio.range %>%
            dplyr::mutate(model = "Pre-asymptotic range", .before = 1),
        dat.moral.numbers.exp8.10.bin.lmer.asymptotic.ratio.range %>%
            dplyr::mutate(model = "Asymptotic range", .before = 1)
    ) %>%
    dplyr::select(-t_log, -p_log) %>%
    rename.terms(term) %>%
    format_exp_info() %>%
    dplyr::arrange(experimentID) %>%
    tidyr::unite(experimentID.model, experimentID, model, sep = " - ") %>%
    fix_zero_p_values(p_or)

dat.moral.numbers.asymptotic.vs.preasymptotic.raw.by.ratio.range.glmm.for.table %>% 
    dplyr::select(-experimentID.model) %>% 
    knitr::kable(
        caption = "Logistic GLMMs for binary acceptability ratings in Experiments 4 and 5, testing the interaction between ratio range (pre-asymptotic vs.\\ asymptotic) and vivacity (neutral vs.\\ vivid). Three model variants are reported for each experiment: using both ratio ranges, the pre-asymptotic range only, and the asymptotic range only. All models include random intercepts for participants and scenarios (except the asymptotic-range model, where the scenario intercept was removed due to missing data). Only data from participants for whom the vivacity manipulation was successful are included.",
        col.names = c("Effect", "Estimate", "SE", "CI", "Estimate", "SE", "CI", "t", "p"),
        booktabs = TRUE,
        longtable = TRUE,
        escape = FALSE
    ) %>%
    kableExtra::add_header_above(c(" " = 1, "Log-odds space" = 3, "Odds-ratio space" = 3, " " = 2)) %>%
    kableExtra::kable_styling(
        latex_options = c(
            "striped",
            "hold_position",
            "repeat_header"
        ),
        font_size = 6
    ) %>%
    kableExtra::kable_classic_2() %>% 
    # For long table, pack_rows must come last
    # See https://github.com/haozhu233/kableExtra/issues/476
    kableExtra::pack_rows(
        index = dat.moral.numbers.asymptotic.vs.preasymptotic.raw.by.ratio.range.glmm.for.table %>% 
            make.pack.index(experimentID.model))




## ----exp8-10-asymptotic-vs-preasymptotic-range-anova---------------------------------------------
dat.moral.numbers.asymptotic.vs.preasymptotic.raw.by.ratio.range %>%
    filter_vivacity_exps() %>%
    combine_exps("^exp(8|10)$") %>%
    tidyr::pivot_longer(
        cols = c(neutral, vivid),
        names_to = "vivacity", values_to = "rating"
    ) %>%
    dplyr::group_by(experimentID) %>%
    dplyr::group_modify(~ {
        aov(rating ~ vivacity * ratio.range + Error(ResponseId / (vivacity * ratio.range)),
            data = .x
        ) %>%
            report.aov(.return.df = TRUE, .print.results = FALSE) %>%
            dplyr::mutate(model.name = unique(.x$experimentID))
    }) %>%
    dplyr::ungroup() %>%
    as.data.frame() %>%
    format_exp_info() %>%
    fix_zero_p_values(p.value) %>%
    kable.packed(
        "experimentID",
        caption = "Repeated-measures ANOVA of raw acceptability difference scores (vivid minus neutral) in Experiments 4 and 5, pooled across participants. The within-subjects factors are vivacity (neutral vs.\\ vivid) and ratio range (pre-asymptotic vs.\\ asymptotic). Only participants for whom the vivacity manipulation was successful are included.",
        booktabs = TRUE,
        longtable = FALSE,
        escape = FALSE
    ) %>%
    kableExtra::kable_classic_2()




## ----save-data-for-prereg, eval = FALSE----------------------------------------------------------
# save(
#     dat.moral.numbers,
#     file = here::here(
#         OUTPUT_DIR,
#         "dat.moral.numbers.RData"
#     )
# )
# 
# 
# save(
#     # This is now dat.moral.numbers.critical.ratios, but we can create the individual data frames by filtering
#     dat.moral.numbers.critical.ratios.exp8,
#     dat.moral.numbers.critical.ratios.exp10,
#     file = here::here(
#         OUTPUT_DIR,
#         "dat.moral.numbers.critical.ratios.exp8_10.RData"
#     )
# )

