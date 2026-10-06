## ----llm-extract-code, eval = FALSE, include = FALSE-------------------------------------------------
# # Extract code to run on the command line, which is much faster than in the GUI
# 
# knitr::purl("moral_numbers_LLMs.Rmd", output = "moral_numbers_LLMs.R")
# 
# lintr::lint("moral_numbers_LLMs.R")


## ----llm-setup, echo = FALSE, include=FALSE, eval = !isTRUE(get0("PARENT_RMD_EXISTS"))---------------
rm (list=ls())

options(digits = 3,
        knitr.kable.NA = '')
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
    fig.align = 'center',
    # Default image width
    out.width = '80%')

# other knits options are here:
# https://yihui.name/knitr/options/


## ----llm-define-parameters-old, eval = !isTRUE(get0("PARENT_RMD_EXISTS"))----------------------------

FIT_PARAMS <- list(
    start = list(w = .6, a = 1),
    lower = c(w = .01, a = 1),
    # This was choosing as the unconstraint a's were well below 2
    upper = c(w = 2, a = 5)
)


EXPERIMENT_DIR <- here::here("experiments")
HELPER_DIR <- here::here("code", "helper_functions")
OUTPUT_DIR <- here::here("code", "output")

# Set seed to Cesar's birthday
set.seed(1207100)


## ----llm-load-libraries, include = FALSE, message = TRUE, warning = TRUE, eval = !isTRUE(get0("PARENT_RMD_EXISTS"))----

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
    # Those are not loaded in moral_numbers.Rmd
    httr,
    ggrepel,    # added: for model labels in joint-preference plots
    
    # Those are
    tidyverse,
    stringr,
    knitr,
    kableExtra,
    rlang,
    ggpubr,
    #xml2,
    XML,
    glue,
   future,    
    furrr,
   ggthemes,
#    xlsx,
#    purrr,
#    Hmisc,
#    stringi,
#    stringdist,
#    pwr
# broom,
#broom.mixed,
)

load_helper_files(HELPER_DIR)

future::plan(multisession, workers = 4)
# #future::plan(multicore, workers = future::availableCores() - 1)
set.seed(1207100)

# Set default theme
#theme_set(theme_linedraw(14))
#theme_set(cowplot::theme_minimal_grid(14))
theme_set(ggthemes::theme_clean(14))
theme_update(legend.position = "bottom",
             legend.justification = "center")


## ----llm-track-correspondance-between-experiments-in-paper-and-in-the-log-files-copied, eval = !isTRUE(get0("PARENT_RMD_EXISTS"))----

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




## ----llm-load-scenarios-copied, eval = !isTRUE(get0("PARENT_RMD_EXISTS"))----------------------------

dat.scenario.files <- tibble::tribble(
    ~experimentID, ~condition, ~file,

    "exp1", NA, here::here(EXPERIMENT_DIR, "experiment1", "scenarios.xml"),
    "exp1b", NA, here::here(EXPERIMENT_DIR, "experiment1b", "scenarios_replication.xml"),
    "exp2", "moral", here::here(EXPERIMENT_DIR, "experiment2_3", "final", "scenarios_xml", "Scenarios_Moral_Choice_Version.xml"),
    "exp2", "economic", here::here(EXPERIMENT_DIR, "experiment2_3", "final", "scenarios_xml", "Scenarios_Economic_Choice_Version.xml"),
    "exp4", "economic", here::here(EXPERIMENT_DIR, "experiment4", "scenarios_replication_economic.xml"),
    # Same as in exp1b
    "exp4", "moral", here::here(EXPERIMENT_DIR, "experiment1b", "scenarios_replication.xml"),
    "exp8", NA, here::here(EXPERIMENT_DIR, "experiment8_incommensurate_scenarios", "incommensurate_scenarios_single_horrificness_question.xml"),
    "exp9a", NA, here::here(EXPERIMENT_DIR, "experiment9_verify_asymptote", "incommensurate_scenarios_single_horrificness_question.10.scenarios.xml"),
    "exp10", NA, here::here(EXPERIMENT_DIR,
                        "experiment10_incommensurate_scenarios_new_ratios", "incommensurate_scenarios_single_horrificness_question.new_questions.scenarios.exp10.xml"),
    "exp11", NA, here::here(EXPERIMENT_DIR, "experiment11_incommensurate_scenarios_new_ratios_replication_of_exp10", "incommensurate_scenarios_single_horrificness_question.new_questions.scenarios.exp10.replication.xml"))

    

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
    dplyr::select(experimentID, condition, title, text, dplyr::starts_with("question"), dplyr::starts_with("option")) %>%
    dplyr::select(-dplyr::matches("anchor")) %>% 
    dplyr::mutate(question.acceptability = dplyr::coalesce(question, question.acceptability)) %>% 
    dplyr::select(-question) %>% 
    dplyr::rename(question.severity = question.severity.text) %>% 
    tidyr::drop_na(title, text) %>% 
    dplyr::mutate(dplyr::across(dplyr::where(is.character), ~ .x %>%
                            #stringr::str_remove_all("\\n") %>%
                             stringr::str_remove_all("[\\t\\n]") %>%
                             stringr::str_remove_all("\\[br\\]") %>%
                             stringr::str_squish()))





## ----llm-load-number-conditions-copied, eval = !isTRUE(get0("PARENT_RMD_EXISTS"))--------------------

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
        
        file_path <- here::here(EXPERIMENT_DIR, relative_path)
        
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
    dplyr::select(experimentID, experimentDesc, Group, dplyr::starts_with("n_")) %>%
    dplyr::mutate(ratio = n_saved / n_victims,
                      #get.number.ratio(n_saved / n_victims, FALSE),
                  .after = "n_total") %>% 
    dplyr::mutate(ratio2 = n_saved2 / n_victims2,
                      #get.number.ratio(n_saved2 / n_victims2, FALSE),
                  .after = "n_total2") 



## ----llm-combine-scenarios-numbers-define-fnc, eval = FALSE------------------------------------------
# 
# # Copied to helper function file
# 
# # 1. Generate Latin Square indices
# generate_latin_square <- function(n = 12, seed = NULL, return_matrix = FALSE) {
# 
#   if (!is.null(seed)) set.seed(seed)
# 
#     dat_lsd <- agricolae::design.lsd(1:n)$sketch %>%
#         as.data.frame() %>%
#         dplyr::mutate(dplyr::across(dplyr::everything(), as.integer))
# 
#     if(isTRUE(return_matrix))
#         dat_lsd <- as.matrix(dat_lsd)
# 
#     dat_lsd
# }
# 
# 
# apply_latin_design <- function(dat1, dat2, dat_lsd) {
# 
#     if(is.list(dat_lsd)){
#         # if a list is provided, we need to extact the correct data frame
#         # based on the number of trials
# 
#         n_trials <- dat1 %>% nrow()
# 
#         dat_lsd <- dat_lsd[[paste0("n", n_trials)]]
#     }
# 
# 
#     purrr::map(seq_len(ncol(dat_lsd)),
#         function(order_idx) {
#             cond_order <- dat_lsd[, order_idx]
#             dplyr::bind_cols(
#                 dat1,
#                 dat2[cond_order, ] %>%
#                     dplyr::mutate(lsd_order = order_idx)
#             )
#         })
# 
# }
# 
# combine_scenarios_and_number_conds <- function(expID){
# 
#     # Assumes that dat.scenario.content and dat.number.conds are global
# 
#     # Fix experimentID for exp2 (where we have exp2b and exp2ac, but the scenarios are the same)
#     expID_scenarios <- ifelse(stringr::str_detect(expID, "exp2"),
#                               "exp2",
#                               expID)
# 
#     # Check if we have multiple conditions in an experiment
#     # If yes, split the data frame, if not, put it into a list anyhow
#     l_scenarios <- dat.scenario.content %>%
#         dplyr::filter(experimentID == expID_scenarios) %>%
#         # Make experiment ID consistent
#         dplyr::mutate(experimentID = expID)
# 
#     if(l_scenarios$condition %>% is.na %>% any){
#         l_scenarios <- list(onlyCond = l_scenarios)
#     } else {
#         l_scenarios <- l_scenarios %>%
#             split(.$condition)
#     }
# 
#     # Check if we have multiple groups for the number conditions
#     # If yes, split the data frame, if not, put it into a list anyhow
#     l_number_conds <- dat.number.conds %>%
#         dplyr::filter(experimentID == expID)
# 
#     if(l_number_conds$Group %>% is.na %>% any){
#         l_number_conds <- list(`Group 0` = l_number_conds)
#     } else {
#         l_number_conds <- l_number_conds %>%
#             split(.$Group)
#     }
# 
#     # Extract names for combos
#     dat_combo_names <- tidyr::expand_grid(
#         experimentID = expID,
#         scenarios = names(l_scenarios),
#         number_conds = names(l_number_conds)
#     ) %>%
#         tidyr::unite(combo_name, experimentID, scenarios, number_conds, sep = "_") %>%
#         dplyr::mutate(combo_name = combo_name %>%
#                            stringr::str_remove_all(" ") %>%
#                            stringr::str_to_lower())
# 
#     tidyr::expand_grid(
#         scenarios = l_scenarios,
#         number_conds = l_number_conds
#     ) %>%
#         dplyr::bind_cols(dat_combo_names)
# 
# }
# 
# #' Combine Multiple Duplicate Columns into One (with Conflict Checking)
# #'
# #' This function identifies all columns in a data frame that match a given base name (e.g., "experimentID"),
# #' checks that their non-NA values are consistent row-wise, and collapses them into a single column.
# #' If conflicting non-NA values are found within a row, the function throws an error.
# #' Redundant copies (e.g., `experimentID...2`, `experimentID...8`) are removed.
# #'
# #' @param dat A data frame or tibble. Defaults to the current data pipe (`.`).
# #' @param col_name A string indicating the base column name to combine (e.g., `"experimentID"`).
# #'
# #' @return A data frame with a single resolved `col_name` column and redundant matching columns removed.
# #' @export
# #'
# #' @examples
# #' df <- tibble::tibble(
# #'   experimentID = c("exp1", "exp2", NA),
# #'   `experimentID...2` = c("exp1", "exp2", "exp3")
# #' )
# #'
# #' combine_multiple_copies_of_columns(df, "experimentID")
# #'
# combine_multiple_copies_of_columns <- function(dat = ., col_name = "experimentID"){
# 
#     dat %>%
#         dplyr::rowwise() %>%
#         dplyr::mutate(!!sym(col_name) := {
#             vals <- dplyr::c_across(dplyr::matches(paste0("^", col_name))) %>% na.omit() %>% unique()
#             if (length(vals) > 1) {
#                 stop(glue::glue("Mismatched {col_name} values: {paste(vals, collapse = ', ')}"))
#             }
#             vals[1]
#         }
#         ) %>%
#         dplyr::select(-dplyr::matches(paste0(col_name, ".+")))
# }
# 


## ----xml-define-function, eval = FALSE---------------------------------------------------------------
# 
# # Copied to helper function file
# 
# xml_to_df <- function(xml_data = ., target_node_name = "scenario"){
# 
#     #top_name <- xml2::xml_name(xml2::xml_root(xml_data))
# 
#     # Find all target nodes (e.g., <scenario>)
#     target_nodes <- xml2::xml_find_all(xml_data, paste0(".//", target_node_name))
# 
#     # Get child node names from the first target node
#     target_sub_node_names <- purrr::map(target_nodes,
#                          ~ {
#                             xml2::xml_children(.x) %>% xml2::xml_name()
#                          })[[1]]
# 
#     # Build a data frame: one row per node
#     purrr::map_dfr(target_nodes,
#                    # for each target node (e.g., scenario)
#                    function(n){
#                        # Loop through the names of the subnotes
#                        purrr::map(target_sub_node_names,
# 
#                                   ~ tibble::tibble(!!rlang::sym(.x) :=
#                                                xml2::xml_find_first(n, paste0("./", .x)) %>%
#                                                xml2::xml_text())) %>%
#                            # Combine the one colum tibbles into a single row
#                            purrr::list_cbind()
#                    })
# 
# }
# 
# 


## ----helper-functions-llm, eval = FALSE--------------------------------------------------------------
# 
# # Copied to helper function
# 
# mfv <- function(x = ., return_number = TRUE){
# 
#     # Get the most frequent value (as a character)
#     val <- names(sort(table(x), decreasing = TRUE))[1]
# 
#     if(isTRUE(return_number)){
#         # Try to convert to numeric if it looks numeric
#         val_num <- suppressWarnings(as.numeric(val))
# 
#         # Return numeric if conversion succeeded
#         if (!is.na(val_num)) {
#             val <- val_num
#         }
#     }
# 
#     return(val)
# }
# 
# classify_models <- function(dat = .) {
# 
#     dat %>%
#         dplyr::mutate(model_type =
#                           dplyr::case_when(
#                               model %in% (dplyr::filter(LLM_LABELS_TIBBLE, model_type == "cloud") %>% pull(model)) ~ "cloud",
#                               model %in% (dplyr::filter(LLM_LABELS_TIBBLE, model_type == "local") %>% pull(model)) ~ "local",
#                               TRUE ~ NA_character_
#                           ),
#                       .before = 1)
# }
# 
# 
# relabel_models <- function(model) {
#     # Apply the label mapping
#     labeled <- LLM_LABELS[model]
# 
#     # Get all possible levels (excluding "Humans")
#     all_levels <- setdiff(unname(LLM_LABELS), "Humans") %>% sort()
# 
#     # Put "Humans" first, then alphabetically sorted others
#     level_order <- c("Humans", all_levels)
# 
#     # Return as factor with specified levels
#     factor(labeled, levels = level_order)
# }
# 
# 
# relabel_conds_llm <- function(cond) {
# 
#     cond <- str_to_title(as.character(cond))
# 
#     cond <- factor(cond,
#                    levels = c("Moral", "Economic",
#                               "Neutral", "Vivid"))
# 
#     cond
# 
# }
# 
# relabel_model_type <- function(x = ., model_type_col = model_type){
# 
#     model_type_col <- enquo(model_type_col)
# 
#     if(is.data.frame(x)){
#         dat <- x
#     } else {
#         dat <- dplyr::tibble(!!quo_name(model_type_col) := x)
#     }
# 
#     dat <- dat %>%
#         dplyr::mutate(
#             !!model_type_col := dplyr::case_when(
#                 is.na(!!model_type_col) ~ "Humans",
#                 !!model_type_col == "cloud" ~ "Frontier models",
#                 !!model_type_col == "local" ~ "Local models",
#             TRUE ~ !!model_type_col
#             ),
#         !!model_type_col := factor(!!model_type_col ,
#                                    levels = c("Humans", "Frontier models", "Local models"))
#         )
# 
#     if(is.data.frame(x)){
# 
#         dat
# 
#     } else {
# 
#         dplyr::pull(dat, !!model_type_col)
# 
#     }
# 
# }
# 
# 


## ----plot-definition-llm-----------------------------------------------------------------------------


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
                levels
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
                function(grp, fctl) {
                    
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



## ----load-human-data, eval = !isTRUE(get0("PARENT_RMD_EXISTS"))--------------------------------------
# Load stuff from the main analysis script

load(here::here(
    OUTPUT_DIR, 
    "tmp_stuff_for_llm.RData")
)



## ----define-parameters-llm---------------------------------------------------------------------------


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

LLMS <- list(
    cloud = c(
        "gpt-4o-mini", # This seems to be completely deontological
        #"gpt-4o",
        #"gpt-4.1", # Not tests
        "gpt-4.1-mini",
        "claude-sonnet-4-5-20250929",
        "gemini-2.5-flash",
        # "gemini-2.5-pro",
        "mistral-medium-latest",
        "deepseek-chat",
        "qwen-plus",
        "gpt-5-mini"
        ),
    local = c("llama3.2",
              "mistral",
              "mistral-nemo")
)

# Note about reasoning effort
# While running the models, we manually changed the reasoning effort in the GPT-5 mini model, and the manually changed the result file
# Our loading procedure uses the tibble below and is thus aware of the reasoning effort. 

LLM_LABELS_TIBBLE <- tibble::tribble(
    ~model_type, ~model, ~label, ~reasoning_effort,
    
    # Cloud models
    "cloud", "gpt-4o-mini", "GPT-4o mini", NA_character_,
    #"gpt-4o",
    #"cloud", "gpt-4.1", "GPT-4.1", NA_character_,
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

LLM_LABELS <- LLM_LABELS_TIBBLE %>% 
    dplyr::select(-reasoning_effort, -model_type) %>% 
    tibble::deframe()
    


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
                        cloud = FALSE)

CHECKPOINT_DIR <- "checkpoints"


## ----combine-scenarios-numbers-----------------------------------------------------------------------



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
                                 stringr::str_replace_all("n_victims", as.character(n_victims)) %>%
                                 stringr::str_replace_all("n_saved", as.character(n_saved)),

                             prompt %>%
                                 stringr::str_replace_all("n_victims2", as.character(n_victims2)) %>%
                                 stringr::str_replace_all("n_saved2", as.character(n_saved2)) %>%
                                 stringr::str_replace_all("n_victims1", as.character(n_victims)) %>%
                                 stringr::str_replace_all("n_saved1", as.character(n_saved))
    )

    prompt <- prompt %>% stringr::str_squish()
    
    
    prompt
    
}
    
dat_scenarios_numbers_flat <- dat_scenarios_numbers %>% 
    purrr::map(function(l){
        purrr::imap_dfr(l, ~ 
                            purrr::list_rbind(.x), .id = "combo_id") %>% 
            dplyr::mutate(
                dplyr::across(dplyr::starts_with("n_"), as.integer),
                prompt = make_prompt(text, question.acceptability, n_saved, n_victims, n_saved2, n_victims2, option1, option2)) %>% 
            combine_multiple_copies_of_columns 
    }) %>% 
    purrr::list_rbind() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(subj = list(subj = glue::glue("{combo_id}_lsd{lsd_order}_s{1:N_SUBJ_LLM_AFTER_LSD}") %>% 
                              as.character())) %>% 
    tidyr::unnest(subj) 
    




## ----api-define-fnc, eval = FALSE--------------------------------------------------------------------
# 
# # Copied to helper function file
# 
# get_model_url <- function(model = .){
# 
#     model <- stringr::str_to_lower(model)
# 
#     if(stringr::str_detect(model, "gpt")) return(LLM_URLS["gpt"] %>% unname)
# 
#     if(stringr::str_detect(model, "claude")) return(LLM_URLS["claude"] %>% unname)
# 
#         # We have different versions of gemini with different urls
#     if(stringr::str_detect(model, "gemini")) return(LLM_URLS[model] %>% unname)
# 
#     if(stringr::str_detect(model, "mistral")) return(LLM_URLS["mistral"] %>% unname)
# 
#     if(stringr::str_detect(model, "deepseek")) return(LLM_URLS["deepseek"] %>% unname)
# 
#     if(stringr::str_detect(model, "qwen")) return(LLM_URLS["qwen-plus"] %>% unname)
# 
#     return(LLM_URLS["ollama"] %>% unname)
# }
# 
# make_llm_header <- function(model) {
# 
#     model_lower <- stringr::str_to_lower(model)
# 
#     # Detect model provider and return appropriate headers
#     if (stringr::str_detect(model_lower, "gpt")) {
#         return(c(Authorization = paste("Bearer", OPENAI_API_KEY)))
# 
#     } else if (stringr::str_detect(model_lower, "claude")) {
#         return(c(
#             `x-api-key` = ANTHROPIC_API_KEY,
#             `anthropic-version` = "2023-06-01"
#         ))
# 
#     } else if (stringr::str_detect(model_lower, "gemini")) {
#         return(c(`x-goog-api-key` = GEMINI_API_KEY))
# 
#     } else if (stringr::str_detect(model_lower, "mistral")) {
#         return(c(Authorization = paste("Bearer", MISTRAL_API_KEY)))
# 
#     } else if (stringr::str_detect(model_lower, "deepseek")) {
#         return(c(Authorization = paste("Bearer", DEEPSEEK_API_KEY)))
# 
#     } else if (stringr::str_detect(model_lower, "qwen")) {
#         return(c(Authorization = paste("Bearer", QWEN_API_KEY)))
# 
#     } else {
#         return(NULL)
#     }
# }
# 
# make_llm_body <- function(model, messages, system_prompt = NULL, system_instruction = NULL) {
#     model_lower <- stringr::str_to_lower(model)
# 
#     # Build body depending on model
#     if (stringr::str_detect(model_lower, "gpt-4")) {
#         return(list(
#             model = model,
#             messages = messages,
#             max_tokens = LLM_OPTIONS$max_output_tokens,        # max tokens to generate in response
#             temperature = LLM_OPTIONS$temperature,        # controls randomness (0 = deterministic)
#             #top_p = LLM_OPTIONS$top_p,              # nucleus sampling (1 = no restriction)
#             n = LLM_OPTIONS$n                  # number of completions to generate
#             # top_k is not a direct parameter in OpenAI API (used internally)
#             #frequency_penalty = 0,  # optional, penalizes frequent tokens
#             #presence_penalty = 0    # optional, penalizes new topic introduction
#         ))
# 
#     } else if (stringr::str_detect(model_lower, "gpt-5")) {
#         return(list(
#             model = model,
#             messages = messages,
#             # reasoning effort: minimal|low|medium|high
#             # default: medium
#             reasoning_effort = "medium",
# 
#             #max_completion_tokens = LLM_OPTIONS$max_output_tokens,        # max tokens to generate in response
#             # Not supported
#             #temperature = LLM_OPTIONS$temperature,        # controls randomness (0 = deterministic)
# 
#             #top_p = LLM_OPTIONS$top_p,              # nucleus sampling (1 = no restriction)
#             n = LLM_OPTIONS$n                  # number of completions to generate
#             # top_k is not a direct parameter in OpenAI API (used internally)
#             #frequency_penalty = 0,  # optional, penalizes frequent tokens
#             #presence_penalty = 0    # optional, penalizes new topic introduction
#         ))
# 
#     } else if (stringr::str_detect(model_lower, "claude")) {
#         return(list(
#             model = model,
#             system = system_prompt,
#             messages = messages,
#             max_tokens = LLM_OPTIONS$max_output_tokens,        # max tokens to generate in response
#             temperature = LLM_OPTIONS$temperature        # controls randomness (0 = deterministic)
#             # can't set top_p togther with temperature
#             #top_p = LLM_OPTIONS$top_p              # nucleus sampling (1 = no restriction)
#             #top_k = LLM_OPTIONS$top_k, # claude supports top_k
#         ))
# 
#     } else if (stringr::str_detect(model_lower, "gemini")) {
#         return(list(
#             #model = model, # spefified in url
#             contents = messages,
#             systemInstruction = system_instruction,
#             generationConfig = list(
#                 temperature = LLM_OPTIONS$temperature,
#                 topP = LLM_OPTIONS$top_p #,
#                 #topK = LLM_OPTIONS$top_k,
#                 #maxOutputTokens = LLM_OPTIONS$max_output_tokens
#             )
#         ))
# 
#     } else if (stringr::str_detect(model_lower, "mistral")) {
#         #https://docs.mistral.ai/api/
#         return(list(
#             model = model,
#             messages = messages,
#             temperature = LLM_OPTIONS$temperature,
#             # Mistral recommends not setting top_p and temperature
#             #top_p = LLM_OPTIONS$top_p,
#             max_tokens = LLM_OPTIONS$max_output_tokens,
#             #presence_penalty = 0,
#             #frequency_penalty = 0,
#             n = LLM_OPTIONS$n,
#             stream = FALSE
#         ))
# 
#     } else if (stringr::str_detect(model_lower, "deepseek")) {
#         # https://api-docs.deepseek.com/
#         return(list(
#             model = model,
#             messages = messages,
#             temperature = LLM_OPTIONS$temperature,
#             # Don't set together with temperature
#             #topP = LLM_OPTIONS$top_p,
#             #frequencyPenalty = 0, # Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim.
#             #presencePenalty = 0, # Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics.
#             maxTokens = LLM_OPTIONS$max_output_tokens,
#             stream = FALSE
#             # stream = TRUE,
#             # streamOptions = list(
#             #     includeUsage = TRUE,
#             #     continuousUsageStats = TRUE
#             # )
#         ))
# 
#     } else if (stringr::str_detect(model_lower, "qwen")) {
#         # https://qwen.ai/apiplatform
#         return(list(
#             model = model,
#             messages = messages,
#             temperature = LLM_OPTIONS$temperature,
#             # Don't set together with temperature
#             #top_p = LLM_OPTIONS$top_p,
#             top_k = LLM_OPTIONS$top_k,
#             #frequency_penalty = 0, # Number between -2.0 and 2.0. Positive values penalize new tokens based on their existing frequency in the text so far, decreasing the model's likelihood to repeat the same line verbatim.
#             #presence_penalty = 0, # Number between -2.0 and 2.0. Positive values penalize new tokens based on whether they appear in the text so far, increasing the model's likelihood to talk about new topics.
#             max_tokens = LLM_OPTIONS$max_output_tokens,
#             n = LLM_OPTIONS$n,
#             stream = FALSE
#             # stream = TRUE,
#             # streamOptions = list(
#             #     includeUsage = TRUE,
#             #     continuousUsageStats = TRUE
#             # )
#         ))
# 
#     } else {
#         # Ollama or default
#         return(list(
#             model = model,
#             messages = messages,
#             options = list(
#                 temperature = LLM_OPTIONS$temperature,
#                 # top_k = LLM_OPTIONS$top_k,
#                 # top_p = LLM_OPTIONS$top_p,
#                 num_ctx = LLM_OPTIONS$num_ctx  # 8k context window
#             ),
#             stream = FALSE
#         ))
#     }
# }
# 
# parse_llm_response <- function(model, response) {
#     model_lower <- stringr::str_to_lower(model)
#     # Parse response depending on model
#     if (stringr::str_detect(model_lower, "gpt")) {
#         result <- content(response)$choices %>%
#             purrr::map_chr(purrr::pluck, "message", "content")
# 
#     } else if (stringr::str_detect(model_lower, "claude")) {
#         result <- purrr::pluck(content(response, as = "parsed"), "content", 1, "text")
# 
#     } else if (stringr::str_detect(model_lower, "gemini")) {
#         result <- purrr::pluck(content(response, as = "parsed"), "candidates", 1, "content", "parts", 1, "text")
# 
#     } else if (stringr::str_detect(model_lower, "mistral|deepseek|qwen")) {
#         result <- purrr::pluck(content(response, as = "parsed"), "choices", 1, "message", "content") %>%
#             stringr::str_trim()
# 
#     } else {
#         result <- purrr::pluck(content(response, as = "parsed"), "message", "content")
#     }
# 
#     return(result)
# }
# 
# send_llm_request <- function(model, url, messages, system_prompt = SYSTEM_PROMPTS$estimation, system_instruction = SYSTEM_INSTRUCTIONS$estimation, DEBUG = FALSE, JUST_COUNT_CHARACTERS = FALSE){
# 
#     model_lower <- stringr::str_to_lower(model)
# 
#     if(isTRUE(JUST_COUNT_CHARACTERS)){
# 
#         if(stringr::str_detect(model, "gemini")){
#             stop("We cannot do the character count for gemini messages.")
#         }
#         n_characters <- return(purrr::map_chr(messages,
#                                    ~ purrr::pluck(.x, "content")) %>%
#                                    stringr::str_length() %>%
#                                    sum %>%
#                                    as.character())
# 
#     }
# 
#      # Build headers depending on model (API)
#     headers <- make_llm_header(model_lower)
# 
#     # Build body depending on model
#     body_json <- make_llm_body(model_lower, messages, system_prompt, system_instruction)
# 
#         if(isTRUE(DEBUG)){
#         cat("\n=== API Request Debug ===\n")
#         print(body_json)
#         cat("==========================\n")
#     }
# 
#     # Now make the request
#     attempt <- 1
#     repeat{
#         response <- httr::RETRY(
#             verb = "POST",
#             url = url,
#             httr::add_headers(.headers = headers),
#             content_type_json(),
#             encode = "json",
#             body = toJSON(body_json, auto_unbox = TRUE),
#             times = LLM_OPTIONS$max_attempts,                  # retry up to 5 times
#             pause_base = 30,
#             pause_min = 10,               # minimum wait between retries (seconds)
#             pause_cap = 60,              # maximum wait
#             terminate_on = c(400, 401)   # don't retry on these status codes
#         )
# 
#          status <- httr::status_code(response)
# 
#          # If 429, handle retry with backoff
#          if (status == 429) {
#              retry_after <- httr::headers(response)[["retry-after"]]
#              wait_time <- if (!is.null(retry_after)) {
#                  as.numeric(retry_after)
#              } else {
#                  #min(2^(attempt - 1) * 5, 60)
#                  30
#              }
#              message(glue::glue("Rate limited (429). Sleeping for {wait_time} seconds (attempt {attempt}/{LLM_OPTIONS$max_attempts})..."))
#              Sys.sleep(wait_time)
#              attempt <- attempt + 1
#              if (attempt > LLM_OPTIONS$max_attempts) {
#                  stop("Max retry attempts reached due to rate limiting (429). Aborting.")
#              }
#              next  # retry loop
#          }
# 
#          # For permanent errors, stop immediately
#          if (status %in% c(400, 401)) {
# 
#              print(glue::glue("API returned status {status}, aborting."))
#              print("\n Full response \n")
#              #print(response)
#              print(content(response, as = "text"))
#              stop("Exiting")
#          }
# 
#          # Break on success or other statuses
#          break
#     }
# 
# 
#     if(isTRUE(DEBUG)){
#         # 🔍 DEBUGGING PRINT
#         cat("\n=== API Response Debug ===\n")
#         cat("\n** Messages **\n")
#         print(messages)
#         cat("\n** Response **\n")
#         print(response)
#         cat("\nStatus Code:", status_code(response), "\n")
#         cat("\nRaw Content:\n")
#         print(content(response, as = "text"))
#         cat("\nSystem Prompt:\n")
#         print(system_prompt)
#         cat("\nSystem instruction:\n")
#         print(system_instruction)
# 
#         cat("==========================\n")
#     }
# 
# 
#     result <- parse_llm_response(model_lower, response)
# 
#     result
# 
# }
# 
# 
# compose_llm_messages <- function(dat = ., model){
#     # dat is a data frane with columns role and message
# 
#     compose_llm_message_inner <- function(role, message, model){
# 
#         l_roles <- list(
#             gemini = c(assistant = "model",
#                        user = "user"),
#             default = c(assistant = "assistant",
#                         user = "user",
#                         system = "system")
#         )
# 
#         # In principle, the format is
#         # list(
#         #     list(role = l_roles[["default"]][role] %>% unname,
#         #          content = message
#         #     )
#         # )
#         # However, as we call the function from within pmap, the outer list is already provided.
#         if(stringr::str_detect(model, "gemini")){
#             #list(
#                 list(role = l_roles[["gemini"]][role] %>% unname,
#                      parts = list(list(text = message))
#                 )
#             #)
#         } else {
#             #list(
#             list(role = l_roles[["default"]][role] %>% unname,
#                  content = message
#             )
#             #)
#         }
# 
#     }
# 
#     dat %>%
#         purrr::pmap(~ compose_llm_message_inner(..1, ..2, model = model))
# }
# 
# get_api_response_incremental <- function(dat = ., model, system_prompt, system_instruction, DEBUG = FALSE, JUST_COUNT_CHARACTERS = FALSE){
# 
# 
#     url <- get_model_url(model)
# 
# 
#     # Get first response. This depends on the model
#     i <- 1
#     if(stringr::str_detect(stringr::str_to_lower(model), "gpt")){
#         # Don't override openai system prompt
#         messages <- tibble::tribble(
#             ~role, ~message,
#             "user", glue::glue("{system_prompt}
# 
#                                {dat$prompt[i]}")
#         ) %>%
#             compose_llm_messages(model = model)
# 
#     } else if(stringr::str_detect(stringr::str_to_lower(model), "gemini|claude")){
# 
#         # System prompt is included in API call
# 
#         messages <- tibble::tribble(
#             ~role, ~message,
#             "user", dat$prompt[i]
#         ) %>%
#             compose_llm_messages(model = model)
# 
#     } else {
#         messages <- tibble::tribble(
#             ~role, ~message,
#             "system", system_prompt,
#             "user", dat$prompt[i]
#         ) %>%
#             compose_llm_messages(model = model)
#     }
# 
#     if(nrow(dat) > 1){
#         messages <- c(messages,
#                       tibble::tribble(
#                           ~role, ~message,
#                           "assistant", dat$response[i]
#                       ) %>%
#                           compose_llm_messages(model = model)
#         )
#     }
# 
#     if(nrow(dat) > 2){
#         # Get subsequent responses except for the very last one
#         messages <- c(messages,
#                       tibble::tibble(
#                           role = rep(c("user", "assistant"), times = nrow(dat) - 2),
#                           message = list(dat$prompt[2:(nrow(dat) - 1)],
#                                       dat$response[2:(nrow(dat) - 1)]) %>%
#                               purrr::pmap(~ c(..1, ..2)) %>%
#                               unlist(use.names = FALSE)
#                       ) %>%
#                           compose_llm_messages(model = model)
#         )
#     }
# 
# 
#     # Last row for which we don't have a response yet
#     if(nrow(dat) > 1){
#         i <- nrow(dat)
#         messages <- c(messages,
#                       tibble::tribble(
#                           ~role, ~message,
#                           "user", dat$prompt[i]
#                       ) %>%
#                           compose_llm_messages(model = model)
#         )
#     }
# 
#     # Send off the response
#     result <- send_llm_request(model, url, messages, system_prompt = system_prompt, system_instruction = system_instruction, DEBUG = DEBUG, JUST_COUNT_CHARACTERS = JUST_COUNT_CHARACTERS) %>%
#          stringr::str_replace("^(\\d+)(\\D*)", "\\1")
# 
#     result
# 
# }
# 
# 
# 
# get_api_response_wrapper_df <- function(dat = ., dat_grp, verbose = FALSE, DEBUG = FALSE, JUST_COUNT_CHARACTERS = FALSE, ...){
# 
#     gc()
# 
# 
#     model <- dat_grp$model %>% unique
#     if(length(model) > 1) stop("More than one model detected")
# 
#     subj <- dat_grp$subj %>% unique
#     if(length(subj) > 1) stop("More than one subject detected")
# 
#     experimentID <- dat_grp$experimentID %>% unique
#     if(length(experimentID) > 1) stop("More than one experiment detected")
# 
#     if(stringr::str_detect(experimentID, "exp2")){
#         system_prompt <- SYSTEM_PROMPTS$comparison
#         system_instruction <- SYSTEM_INSTRUCTIONS$comparison
#     } else {
#         system_prompt <- SYSTEM_PROMPTS$estimation
#         system_instruction <- SYSTEM_INSTRUCTIONS$estimation
#     }
# 
#     if(isTRUE(verbose))
#         print(glue::glue("Running model {model} for experiment {experimentID} and subject {subj}."))
# 
# 
#     # Prepare return df
#     dat <- dat %>%
#         dplyr::mutate(response = NA_character_)
# 
#     for(i in seq_len(nrow(dat))){
#         dat[i, "response"] <- tryCatch({
#                            res <- get_api_response_incremental(
#                                dat[seq_len(i),],
#                                model = model,
#                                system_prompt = system_prompt,
#                                system_instruction = system_instruction,
#                                DEBUG = DEBUG,
#                                JUST_COUNT_CHARACTERS = JUST_COUNT_CHARACTERS
#                                )
#                            if (length(res) == 0) NA_character_ else res
#                            },
# 
#                            error = function(e) NA_character_
#                        )
# 
#     }
# 
# 
# 
#     return(dat)
# }
# 


## ----test-api, eval = FALSE--------------------------------------------------------------------------
# 
# purrr::map_dfr(c(
#     # "gpt-4o",
#     #"claude-sonnet-4-5-20250929"
#     # "gpt-4.1-mini",
#     #"mistral-medium-latest"
#      #"gpt-5-mini"
#     # "deepseek-chat",
#     # "qwen-plus"
#     # For gemini, system prompt is already included in API call.
#     # Comment it out below
#     # LLMS$cloud[4], #"gemini-2.5-flash"
#     # LLMS$cloud[5] #"gemini-2.5-pro"
# ),
# function(m){
# 
#     url <- get_model_url(m)
# 
#     message <- tibble::tribble(
#         ~role, ~message,
#         # Don't include for gemini and claude
#         "system", "just answer with numbers, anything else makes your output unusable.",
#         "user", "what is the capital of england: 1 = bristol, 2 = leeds, 3 = london, 4 = paris, 5=hawai?") %>%
#         compose_llm_messages(model = m)
#     #message
# 
# 
#     response <- tryCatch(
#         send_llm_request(model = m, url = url, messages = message, DEBUG = TRUE),
#         error = function(e) {
#             message("Error in send_llm_request: ", e$message)
#             NA_character_
#         }
#     )
# 
#     data.frame(
#         model = m,
#         response = response
#     )
# }
# ) -> llm_test_responses
# 
# 
# 


## ----count-tokens, eval = FALSE----------------------------------------------------------------------
# 
# dat_llm_results_exps_characters <- dat_scenarios_numbers_flat %>%
#         tidyr::expand_grid(model = LLMS$local[1]) %>%
#         dplyr::group_by(model, experimentID, experimentDesc, condition, Group, lsd_order, combo_id, subj) %>%
#         dplyr::group_modify(~ get_api_response_wrapper_df(.x, .y, verbose = FALSE, DEBUG = FALSE, JUST_COUNT_CHARACTERS = TRUE),
#                             .keep = FALSE) %>%
#         tidyr::unnest(response) %>%
#         dplyr::mutate(response = as.numeric(response))
# 
# (dat_llm_results_exps_characters$response %>% sum)/4
# # 94,623,050 characters, 23,655,762
# # We just need at least 23M tokens for each model!


## ----LLM-option-augment------------------------------------------------------------------------------

llm_option_metadata <- list(
    max_attempts = list(
        description  = "Max. API call retries on failure",
        available_in = "All models (client-side)"
    ),
    max_output_tokens = list(
        description  = "Max. tokens in model response",
        available_in = "GPT-4, Claude, Mistral, Deepseek, Qwen"
    ),
    temperature = list(
        description  = "Response randomness (0 = deterministic)",
        available_in = "GPT-4, Claude, Gemini, Mistral, Deepseek, Qwen, Local"
    ),
    top_k = list(
        description  = "Top-K token sampling cutoff",
        available_in = "Qwen"
    ),
    top_p = list(
        description  = "Nucleus sampling probability",
        available_in = "Gemini"
    ),
    n = list(
        description  = "Number of completions per request",
        available_in = "GPT-4, GPT-5, Mistral, Qwen"
    ),
    frequency_penalty = list(
        description  = "Penalizes already-frequent tokens",
        available_in = "---"
    ),
    presence_penalty = list(
        description  = "Penalizes tokens seen so far",
        available_in = "---"
    ),
    num_ctx = list(
        description  = "Context window size (tokens)",
        available_in = "Local"
    )
)

# Safety check: all LLM_OPTIONS keys must be covered by the metadata list
stopifnot(
    "Some LLM_OPTIONS parameters are missing from llm_option_metadata" =
        all(names(LLM_OPTIONS) %in% names(llm_option_metadata))
)




## ----LLM-option-print--------------------------------------------------------------------------------


# Build result tibble: LLM_OPTIONS values joined with metadata descriptions
LLM_OPTIONS %>%
    unlist() %>%
    tibble::enframe(name = "parameter", value = "value") %>%
    dplyr::left_join(
        llm_option_metadata %>%
            tibble::enframe(name = "parameter") %>%
            tidyr::unnest_wider(value),
        by = "parameter"
    ) %>%
    dplyr::mutate(parameter = stringr::str_replace_all(parameter, "_", "\\\\_")) %>%
    dplyr::rename(
        Parameter     = parameter,
        Value         = value,
        Description   = description,
        `Available in` = available_in
    ) %>%
    knitr::kable(
        caption = "API parameters and the models for which each was included in the request body. GPT-4 = GPT-4o mini, GPT-4.1, GPT-4.1 mini; GPT-5 = GPT-5 mini (all reasoning effort levels); Local = Llama 3.2, Mistral 7.2B, Mistral nemo (Ollama). Dashes indicate parameters defined in \\texttt{LLM\\_OPTIONS} but not passed to any model. \\texttt{max\\_output\\_tokens} is passed under model-specific field names (\\texttt{max\\_tokens} for GPT-4, Claude, Mistral, Qwen; \\texttt{maxTokens} for Deepseek).",
        booktabs = TRUE,
        longtable = FALSE,
        escape = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::kable_styling(latex_options = c("striped",
        "scale_down",
        "hold_position")) %>%
    kableExtra::kable_classic_2()



## ----LLM-system-prompt-print-------------------------------------------------------------------------


SYSTEM_PROMPTS %>% 
    purrr::map(~ stringr::str_replace_all(.x, "\n", " \\\\newline ")) %>%
    tibble::as_tibble_row() %>% 
    knitr::kable(
        caption = "System prompts used in all experiments, giving the models general instructions on how to respond.",
        col.names = c(
            "Rating on 6-point scale (all experiments but Exp. 3)",
            "Choice between two situations (Exp. 3)"
        ),
        booktabs = TRUE,
        longtable = FALSE,
        escape = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::column_spec(1:2, width = "0.5\\\\linewidth") %>%
        kableExtra::kable_styling(latex_options = c(
        "hold_position"
        )) %>%
    kableExtra::kable_classic_2()
    

    


## ----number-of-trials-print--------------------------------------------------------------------------

dat_llm_n_trials_responses <- purrr::reduce(
    list(
   
        # Cells in latin square
        dat_scenarios_numbers_flat %>% 
            dplyr::distinct(experimentID, condition, Group, lsd_order)  %>% 
            dplyr::count(experimentID, condition, Group)  %>% 
            dplyr::rename("Number of cells" = n),
        
        # Number of trial per subject
        dat_scenarios_numbers_flat %>% 
            dplyr::count(experimentID, condition, Group, subj)  %>% 
            dplyr::distinct(experimentID, condition, Group, n) %>% 
            dplyr::rename("Number of trials/participant" = n),
        
        # Number of Responses
        dat_scenarios_numbers_flat %>%
            dplyr::count(experimentID, condition, Group) %>% 
            dplyr::rename("Number of responses" = n)
    ),
    ~ dplyr::left_join(
        .x, 
        .y, 
        by = c("experimentID", "condition", "Group")
    )   
) %>% 
    dplyr::mutate("Number of participants" = N_SUBJ_LLM_AFTER_LSD * `Number of cells`,
                  .after = `Number of cells`) 

dat_llm_n_trials_responses %>% 
    format_exp_info() %>% 
    dplyr::mutate(condition = relabel_conds_llm(condition)) %>% 
    dplyr::rename_with(~ stringr::str_remove(.x, "Number of"),
                       dplyr::starts_with("Number of")) %>%
    dplyr::arrange(experimentID, condition, Group) %>% 
    dplyr::rename_with(~ stringr::str_to_title(.x)) %>%
    kable.packed(
        "Experimentid", # Due to rename_width
        caption = "Experimental design summary for LLM simulations. Each cell in the Latin square design was tested with 10 independent response sets, with each response treated as a simulated participant. The table shows the number of conditions (cells), simulated participants, trials per participant, and total responses for each experiment. In Experiment 6, \\emph{Group} refers to two different sets of victim and beneficiary numbers crossed with the neutral/vivid manipulation, resulting in four conditions total.",
        #col.names = en_math_col_names(., 1),
        booktabs = TRUE,
        longtable = FALSE,
        escape = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::add_header_above(c(" " = 2, "Number of" = 4), bold = TRUE) %>%
    kableExtra::kable_styling(latex_options = c("striped",
        "scale_down",
        "hold_position",
        "repeat_header")) %>% 
    kableExtra::kable_classic_2()
    



## ----run-exp-----------------------------------------------------------------------------------------




if(isTRUE(RECALCULATE_LLM$local)){

    # When run through Rscript, llama3.2 takes 684 s/11.4 min per subject (after application of the latin square design, so 10 or 12 subjects in total, depending on the experiment).
    # With 10 replicates of each cell (i.e., 100 or 120 subjects), this would take roughly 2h per model, and 4h for llama and mistral-nemo

    dat_llm_results_exps_local <- dat_scenarios_numbers_flat %>% 
        tidyr::expand_grid(model = LLMS$local) %>% 
        dplyr::group_by(model, experimentID, experimentDesc, condition, Group, lsd_order, combo_id, subj) %>% 
        dplyr::group_modify(~ get_api_response_wrapper_df(.x, .y, verbose = TRUE, DEBUG = FALSE),
                            .keep = FALSE) %>% 
        tidyr::unnest(response) %>% 
        dplyr::mutate(response = as.numeric(response)) %>% 
        dplyr::rename(rating.raw = response) %>% 
        dplyr::rename_with(~ stringr::str_replace_all(.x, "_", "."), dplyr::matches("n_"))  %>%
        tidyr::drop_na(rating.raw) %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(c("n.victims", "n.saved")), as.integer),
                      rating.bin = 1 * (rating.raw > 3),
                      n.net.saved =  n.saved - n.victims,
                      n.total = n.victims + n.saved)

    save(
        dat_llm_results_exps_local,
        file = here::here(
            OUTPUT_DIR,
            "llm_test.all_exps.local_models.RData"
            )
        )
} else {
    load(here::here(
        OUTPUT_DIR,
        "llm_test.all_exps.local_models.RData"
    )
    )
}
 

if(isTRUE(RECALCULATE_LLM$cloud)){   
    
    # Parellelize only by model
    # We save model specific RData files from within the loop as well as a general RData file with some reformatting
    
    safe_get_api_response <- purrr::safely(get_api_response_wrapper_df, otherwise = NULL)
    
    dat_llm_results_exps_cloud <- furrr::future_map_dfr(
        LLMS$cloud,
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
                 file = here::here(
                     OUTPUT_DIR,
                     result_file_name
                 )
            )
            
            partial_result
        }
        
    ) %>% 
        tidyr::unnest(response, keep_empty = TRUE) %>% 
        dplyr::mutate(response = as.numeric(response)) %>% 
        dplyr::rename(rating.raw = response) %>% 
        dplyr::rename_with(~ stringr::str_replace_all(.x, "_", "."), dplyr::matches("n_"))  %>%
        tidyr::drop_na(rating.raw) %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(c("n.victims", "n.saved")), as.integer),
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
    
    
    l_cloud_result_vars <- 
        purrr::map(
            LLMS$cloud,
            function(model){
                
                file_name <- glue::glue("llm_test.all_exps.{model}.RData")     
                
                file <- here::here(OUTPUT_DIR, file_name)
            
                if(!file.exists(file)) return(NA_character_)
            
                tmp_env <- new.env()
                load(file, envir = tmp_env)
                var_names_in_env <- ls(tmp_env) 
            
                purrr::walk(var_names_in_env,
                     ~ assign(.x, tmp_env[[.x]], envir = .GlobalEnv)
                )
                
            var_names_in_env
        }
    ) %>% 
        purrr::flatten_chr() %>% 
        purrr::discard(is.na)
    


        
}





## ----combine-llm-results-----------------------------------------------------------------------------

# Rbind results
dat_llm_results_exps <- dplyr::bind_rows(
    dat_llm_results_exps_local %>% 
        ungroup(),

    purrr::map(l_cloud_result_vars,
        ~ get(.x, envir = .GlobalEnv) %>% 
            dplyr::ungroup()
    ) %>%
        purrr::list_rbind() %>% 
        # Forgot to add this to saved files for individual models
        tidyr::unnest(response, keep_empty = TRUE) %>% 
        dplyr::mutate(response = as.numeric(response),
                      response = ifelse(dplyr::between(response, 1, 6),
                                        response,
                                        NA_real_)) %>% 
        dplyr::rename(rating.raw = response) %>% 
        dplyr::rename_with(~ stringr::str_replace_all(.x, "_", "."), dplyr::matches("n_"))  %>%
        tidyr::drop_na(rating.raw) %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(c("n.victims", "n.saved")), as.integer),
                      rating.bin = 1 * (rating.raw > 3),
                      n.net.saved =  n.saved - n.victims,
                      n.total = n.victims + n.saved)
) %>%
# For compatibility with the human experiments
dplyr::rename(ResponseId = subj)


## ----invert-rartings-exp2-llm------------------------------------------------------------------------

# In the human experiments, 6 means the high ratio option is code as the second option (though the option order is counterbalanced, here it's the first)

dat_llm_results_exps <- dat_llm_results_exps %>% 
    dplyr::mutate(rating.bin = dplyr::if_else(stringr::str_detect(experimentID, "exp2"),
                                               1 - rating.bin, rating.bin),
                  rating.raw = dplyr::if_else(stringr::str_detect(experimentID, "exp2"),
                                               7 - rating.raw, rating.raw)
    ) 




## ----verify-llm-results------------------------------------------------------------------------------

dplyr::bind_rows(
    dat_llm_results_exps %>% 
        dplyr::mutate(response_valid = is.finite(rating.bin) & is.finite(rating.raw)) %>% 
        dplyr::filter(response_valid,
                      model != "participants") %>% 
        dplyr::count(experimentID, condition, model) %>% 
        dplyr::group_by(experimentID, condition) %>% 
        dplyr::summarize(
            mfv = mfv(n),
            min = min(n),
            max = max(n),
            max_p_data_loss = 100 * (mfv - min) / mfv,
            .groups = "drop") %>% 
        dplyr::mutate(choosen_models = "All models"),
    
    dat_llm_results_exps %>% 
        dplyr::mutate(response_valid = is.finite(rating.bin) & is.finite(rating.raw)) %>% 
        dplyr::filter(response_valid,
                      !stringr::str_detect(model, "claude"),
                      model != "participants") %>% 
        dplyr::count(experimentID, condition, model) %>% 
        dplyr::group_by(experimentID, condition) %>% 
        dplyr::summarize(
            mfv = mfv(n),
            min = min(n),
            max = max(n),
            max_p_data_loss = 100 * (mfv - min) / mfv,
            .groups = "drop") %>% 
        dplyr::mutate(choosen_models = "All models excluding Claude Sonnet 4.5")
) %>% 
    dplyr::left_join(
        dat_llm_n_trials_responses %>% 
            dplyr::group_by(experimentID, condition) %>% 
            dplyr::summarise(expected = sum(`Number of responses`),
                             .groups = "drop"),
        by = c("experimentID", "condition")
    ) %>% 
    dplyr::relocate(expected, .before = mfv) %>% 
    format_exp_info() %>% 
    dplyr::mutate(condition = relabel_conds_llm(condition)) %>% 
    dplyr::arrange(choosen_models, experimentID, condition) %>% 
    kable.packed(
        "choosen_models",
    caption = "Valid response rates across experiments and models. Valid responses were numerical scores (1-6) with no additional text. Maximum data loss (excluding Claude Sonnet 4.5) was 1.5\\%. Claude Sonnet 4.5 showed substantial data loss in economic conditions of Experiments 3a and 3b, returning undefined responses or extraneous content beyond the requested numerical values. This pattern replicated across independent simulations, suggesting systematic difficulties following instructions in these specific conditions.",
        col.names = c("Experiment", "Condition", "Expected value", "Most frequent value", "Min", "Max", "Max data loss (\\%)"),
        booktabs = TRUE,
#        longtable = TRUE,
        escape = FALSE) %>%
    kableExtra::row_spec(0, bold = TRUE) %>%
    kableExtra::kable_styling(latex_options = c("striped",
        "scale_down",
        "hold_position",
        "repeat_header")) %>% 
    kableExtra::kable_classic_2()




## ----add-human-to-llm-results------------------------------------------------------------------------

dat_llm_results_exps <- dplyr::bind_rows(
    dat_llm_results_exps,
    
    dat.moral.numbers %>% 
        # Select relevant experiments
        dplyr::filter(experimentID %in% c("exp10.vivacityManipulationWorking",
                                          "exp4",
                                          "exp2a",
                                          "exp2b",
                                          "exp2c"),
                      question == "acceptability") %>% 
        # Combining Experiments 2a and 2c as they are replications of one another
        combine_exps("exp2[ac]") %>%
        dplyr::mutate(experimentID = ifelse(experimentID == "exp10.vivacityManipulationWorking",
                                            "exp11", experimentID)) %>% 
        dplyr::select(experimentID, decisionType, vivacity, ResponseId,
                      dplyr::matches("^(n\\.|ratio|rating)"))  %>% 
        # Make consistent with LLM format
        dplyr::mutate(n.saved = dplyr::coalesce(n.saved, n.saved1)) %>%
        dplyr::mutate(n.victims = dplyr::coalesce(n.victims, n.victims1)) %>%
        dplyr::mutate(n.total = dplyr::coalesce(n.total, n.total1)) %>%
        dplyr::mutate(ratio = dplyr::coalesce(ratio, ratio1)) %>%
        dplyr::mutate(n.net.saved = dplyr::coalesce(n.net.saved, n.net.saved1)) %>%
        dplyr::select(-dplyr::ends_with("1"), -c(ratio.of.net.saved, ratioOrder, ratio.theoretical, ratio.of.ratios)) %>%
        dplyr::mutate(condition = dplyr::coalesce(decisionType, vivacity)) %>% 
        dplyr::select(-decisionType, -vivacity) %>% 
        dplyr::mutate(model = "participants") 
)





## ----combine-llm-results-make-long-------------------------------------------------------------------

# Make long to have separate columns for raw and bin

dat_llm_results_exps_long <- dat_llm_results_exps %>% 
    tidyr::pivot_longer(starts_with("rating"),
                  names_to = "measure",
                  values_to = "rating") %>% 
    dplyr::mutate(measure = stringr::str_remove(measure, "rating.")) %>%
    dplyr::mutate(chance.level = ifelse(measure == "bin",
                                        .5,
                                        3.5))

# Find all ratios by experiment
l_llm_ratios_by_exp <- dat_llm_results_exps %>% 
  dplyr::distinct(experimentID, ratio) %>%
  dplyr::group_by(experimentID) %>%
  dplyr::summarise(ratios = list(sort(ratio)), .groups = "drop") %>%
  tibble::deframe()


## ----llm-make-fit------------------------------------------------------------------------------------




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



dat_llm_fits_1_param <- dat_llm_results_exps_long %>% 
    dplyr::filter(measure == "bin",
                  experimentID %in% c("exp4", "exp11")) %>% 
    dplyr::group_by(model, experimentID, condition, n.victims, n.saved, ratio) %>% 
    dplyr::summarise(rating = mean(rating), .groups = "drop") %>% 
    dplyr::group_by(model, experimentID, condition) %>% 
    dplyr::group_modify(~ tryCatch(
        minpack.lm::nlsLM(
            rating ~ acceptability.fnc (w, 
                                            n.saved, n.victims),
            data = .x,
            # This is a list
            start = FIT_PARAMS$start["w"],
            #nlsLM specific options
            # These are vectors
            lower = FIT_PARAMS$lower["w"],
            upper = FIT_PARAMS$upper["w"]) %>% 
            broom::tidy(),
        error = function(e) data.frame(
            term = c("w")) %>% 
            dplyr::mutate(
                estimate = NA,
                std.error = NA,
                statistic = NA,
                p.value = NA
            )
    )) %>% 
    # Just in case
    dplyr::ungroup()
    

dat_llm_fits_2_param <- dat_llm_results_exps_long %>% 
    dplyr::filter(measure == "bin",
                  experimentID %in% c("exp4", "exp11")) %>% 
    dplyr::group_by(model, experimentID, condition, n.victims, n.saved, ratio) %>% 
    dplyr::summarise(rating = mean(rating), .groups = "drop") %>% 
    dplyr::group_by(model, experimentID, condition) %>% 
    dplyr::group_modify(~ tryCatch(
        minpack.lm::nlsLM(
            rating ~ acceptability.fnc (w, 
                                        n.saved, n.victims,
                                        a = a),
            data = .x,
            # This is a list
            start = FIT_PARAMS$start,
            #nlsLM specific options
            # These are vectors
            lower = FIT_PARAMS$lower,
            upper = FIT_PARAMS$upper) %>%  
            broom::tidy(),
        error = function(e) data.frame(
            term = c("w", "a")) %>% 
                dplyr::mutate(
                    estimate = NA,
                    std.error = NA,
                    statistic = NA,
                    p.value = NA
        )
    )
    ) %>% 
    # Just in case
    dplyr::ungroup()
            


## ----plot-llm-results-prepare-single-option-responses------------------------------------------------

p_list_llm_single_option <- purrr::map(
    c("cloud", "local") %>% purrr::set_names(.),
    function(mdl_type){
        
        v_allowed_models <- c(
            LLM_LABELS_TIBBLE %>%
                dplyr::filter(model_type == mdl_type) %>%
                dplyr::pull(model),
            "participants"
        )

        purrr::map(
            c("exp4", "exp11") %>% purrr::set_names(.),
            function(expID){
                
                purrr::map(
                    c("raw", "bin") %>% purrr::set_names(.),
                    function(meas){
                        

                        if(meas == "bin"){
                            
                            dat_fit <- get_dat_fit_for_plot_llm(
                                experimentID == expID,  
                                #model == mod,
                                n_fit_params = ifelse(
                                    expID == "exp11",
                                    2, 1)
                            ) %>% 
                                dplyr::filter(model %in% v_allowed_models) %>% 
                                dplyr::mutate(
                                    condition = relabel_conds_llm(condition),
                                    model = relabel_models(model)
                                )
                            
                            
                            add_fit = TRUE
                            
                        } else {
                            dat_fit <- NULL
                            add_fit = FALSE
                        }
                        
                        dat_llm_results_exps_long %>% 
                            dplyr::filter(experimentID == expID,
                                          measure == meas) %>% 
                            dplyr::filter(model %in% v_allowed_models) %>% 
                            format_exp_info() %>% 
                            dplyr::mutate(experimentID = stringr::str_to_sentence(experimentID),
                                          condition = relabel_conds_llm(condition),
                                          model = relabel_models(model)) %>% 
                            create.predictor.comparison.plot.llm(
                                facet.var = condition, 
                                value.var = rating,
                                col.var = model,
                                dat.fit = dat_fit,
                                add.fit = add_fit,
                                ylab = "Rating",
                                legend = "bottom",
                                return.plot = FALSE,
                                add.p.saved.to.list = FALSE
                            ) %>% 
                            purrr::map(
                                ~ .x + 
                                    # geom_smooth(method = "loess", se = FALSE) + 
                                    ggplot2::facet_grid(condition ~ model,
                                               labeller = ggplot2::label_wrap_gen(width = 12)) +
                                    ggplot2::theme(legend.position = "none")
                                # Reset colors without changing the grouping or the function
                                #scale_color_manual(values = rep("black", 100)) 
                                #~ .x + geom_line()
                            ) %>% 
                            purrr::map(
                                ~ {
                                    if(expID == "exp11"){
                                        .x +
                                            ggplot2::scale_x_continuous(
                                                breaks = l_llm_ratios_by_exp$exp11,
                                                trans = "log2",
                                                guide = ggplot2::guide_axis(angle = 60,
                                                                   check.overlap = TRUE))
                                    } else {
                                        .x +
                                            ggplot2::scale_x_continuous(
                                                trans = "log2",
                                                guide = ggplot2::guide_axis(angle = 60,
                                                                   check.overlap = TRUE))
                                        
                                    }
                                }
                            )
                    }
                )
            }
        ) 
    }
)
        


