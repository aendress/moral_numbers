## ----llm-extract-code, eval = FALSE, include = FALSE---------------------------------------
# # Extract code to run on the command line, which is much faster than in the GUI
# 
# knitr::purl("moral_numbers_LLMs_exp3_rerun.Rmd",
#             output = "moral_numbers_LLMs_exp3_rerun.R")
# 
# lintr::lint("moral_numbers_LLMs_exp3_rerun.R")


## ----exp3-rerun-guard, include = FALSE-----------------------------------------------------
# This file reruns ONLY Experiment 3 (exp2b = Exp. 3a, exp2ac = Exp. 3b) with the corrected
# comparison prompt. The original comparison prompt had both scale anchors pointing towards Option 2
# ("1 = Option 1 is much more unacceptable, 6 = Option 2 is much more acceptable").
# It is standalone and must never be knitted as a child of moral_numbers.Rmd.
# Results go to new files (llm_exp3_fixed_prompt.*.RData); the llm_test.all_exps.*.RData files are never touched.

if (isTRUE(get0("PARENT_RMD_EXISTS"))) {
    stop("moral_numbers_LLMs_exp3_rerun.Rmd is standalone and must not be knitted as a child document.")
}

OUTPUT_FILE_PREFIX <- "llm_exp3_fixed_prompt"
EXP3_IDS <- c("exp2b", "exp2ac")


## ----llm-setup, echo = FALSE, include=FALSE, eval = !isTRUE(get0("PARENT_RMD_EXISTS"))-----
#rm (list=ls())

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


## ----llm-define-parameters-old, eval = !isTRUE(get0("PARENT_RMD_EXISTS"))------------------

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
    jsonlite,   # added for the rerun (send_llm_request() used to call toJSON() unqualified)
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


## ----exp3-rerun-define-api-functions-------------------------------------------------------
# Request bodies for the rerun: make_llm_body_reasoning() in helper_functions/moral_numbers_LLM_API_functions.R
# (previously defined here as make_llm_body_exp3()). Used via send_llm_request(..., body_fn = make_llm_body_reasoning),
# which passes the reasoning effort (column reasoning_effort of the run table) as fifth argument.
# All other API functions are the ones in helper_functions/moral_numbers_LLM_API_functions.R.
# Check the parameter names for the new models with the test-api chunk before running the full simulations.


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




## ----llm-load-scenarios-copied, eval = !isTRUE(get0("PARENT_RMD_EXISTS"))------------------

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





## ----exp3-rerun-fix-aqueduct-scenario------------------------------------------------------
# The economic version of the "Aqueduct in the desert" scenario (Exp. 3) names the wrong farm in option 2
# ("... save n_saved2 acres of crops in the Sandharbor farm" instead of the Blackfield farm).
# Human participants saw the erroneous text; the rerun uses the corrected text.

stopifnot(
    "Expected exactly one economic Aqueduct scenario with the wrong farm in option 2" =
        dat.scenario.content %>%
        dplyr::filter(
            experimentID == "exp2",
            condition == "economic",
            title == "Aqueduct in the desert",
            stringr::str_detect(option2, "acres of crops in the Sandharbor farm")
        ) %>%
        nrow() == 1
)

dat.scenario.content <- dat.scenario.content %>%
    dplyr::mutate(
        option2 = dplyr::if_else(
            experimentID == "exp2" & condition == "economic" & title == "Aqueduct in the desert",
            stringr::str_replace(option2, "acres of crops in the Sandharbor farm", "acres of crops in the Blackfield farm"),
            option2
        )
    )

stopifnot(
    "Aqueduct correction failed" =
        !any(stringr::str_detect(dat.scenario.content$option2, "acres of crops in the Sandharbor farm"),
             na.rm = TRUE)
)


## ----exp3-rerun-counterbalance-option-order------------------------------------------------
# As in the human experiments, the order of the two options is counterbalanced across (simulated) participants.
# In the scenario files, the high-ratio option is option 1. We add a copy of the Experiment 3 scenarios with the
# options switched (high-ratio option second). Each option text contains its own number placeholders
# (n_saved1, n_victims1 vs. n_saved2, n_victims2), so the numbers move with the text.
# The option order is appended to the condition (e.g., moral.high_ratio_first), so that each option order gets its
# own Latin square and simulated participants, and is split off into ratioOrder in combine-scenarios-numbers.
n_exp2_scenarios <- sum(dat.scenario.content$experimentID == "exp2")

dat.scenario.content <- dplyr::bind_rows(
    dat.scenario.content %>%
        dplyr::filter(experimentID != "exp2"),
    dat.scenario.content %>%
        dplyr::filter(experimentID == "exp2") %>%
        dplyr::mutate(condition = stringr::str_c(condition, ".high_ratio_first")),
    dat.scenario.content %>%
        dplyr::filter(experimentID == "exp2") %>%
        dplyr::mutate(
            condition = stringr::str_c(condition, ".high_ratio_second"),
            option1_original = option1,
            option1 = option2,
            option2 = option1_original
        ) %>%
        dplyr::select(-option1_original)
)

stopifnot(
    "Expected two option orders for each Experiment 3 scenario" =
        sum(dat.scenario.content$experimentID == "exp2") == 2 * n_exp2_scenarios
)


## ----exp3-rerun-option-labels--------------------------------------------------------------
# The options are labelled A and B instead of 1 and 2, so that the option labels cannot be confused with the values
# of the 6-point scale (see SYSTEM_PROMPTS$comparison): with "option 1" and "option 2", several models answered "2"
# to mean Option 2. Here, the question of the Experiment 3 scenarios is changed; the labels before the options are
# set by make_prompt(..., option_labels = c("A", "B")) in combine-scenarios-numbers.
EXP3_QUESTION_12 <- "If you were faced with this choice, would you choose option 1 or option 2?"
EXP3_QUESTION_AB <- "If you were faced with this choice, would you choose option A or option B?"

# All acceptability questions of the Experiment 3 scenarios must have exactly this wording
stopifnot(
    "Not all Experiment 3 acceptability questions have the expected wording" =
        all(dat.scenario.content$question.acceptability[dat.scenario.content$experimentID == "exp2"] == EXP3_QUESTION_12)
)

dat.scenario.content <- dat.scenario.content %>%
    dplyr::mutate(
        question.acceptability = dplyr::if_else(
            experimentID == "exp2",
            EXP3_QUESTION_AB,
            question.acceptability
        )
    )

stopifnot(
    "Not all Experiment 3 acceptability questions were changed" =
        all(dat.scenario.content$question.acceptability[dat.scenario.content$experimentID == "exp2"] == EXP3_QUESTION_AB)
)


## ----llm-load-number-conditions-copied, eval = !isTRUE(get0("PARENT_RMD_EXISTS"))----------

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
    purrr::pmap_dfr(\(relative_path, experimentID, experimentDesc, Group, rename_victims) {
        
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



## ----load-human-data, eval = !isTRUE(get0("PARENT_RMD_EXISTS"))----------------------------
# Load stuff from the main analysis script

load(here::here(
    OUTPUT_DIR, 
    "human_data_for_llm_analyses.RData")
)



## ----define-parameters-llm-----------------------------------------------------------------


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
    # Corrected prompt. The original prompt had both anchors pointing towards Option 2:
    # "When asked about the relative acceptability of two actions, respond on a 6-point scale (1 = Option 1 is much more unacceptable, 6 = Option 2 is much more acceptable)."
    # The new anchors follow the human experiments ("1: Definitely Option 1" ... "6: Definitely Option 2"), but the
    # options are labelled A and B (see exp3-rerun-option-labels): with "Option 1" and "Option 2", models answered
    # "2" to mean Option 2, although 2 is on the Option 1 side of the scale.
    comparison = "When asked to choose between two options, respond on a 6-point scale (1 = definitely Option A, 6 = definitely Option B)."
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
    # Used for new models with reasoning switched on, where the reasoning tokens count towards the limit
    max_output_tokens_reasoning = 16000,
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

# Note about reasoning effort
# In the original simulations, we manually changed the reasoning effort in the GPT-5 mini model and the result files.
# Here, model and reasoning effort are separate columns; each cloud run is one model x reasoning effort combination,
# and the reasoning effort is passed to the API through make_llm_body_reasoning().
# The rerun uses the current mid-tier models of each provider instead of the original (partly retired) models
# (status as of 26 Oct 2026):
# - mistral-medium-latest now points to Mistral Medium 3.5 (Medium 3 retired 31 Aug 2026)
# - deepseek-flash (V4.1 Flash) replaces deepseek-chat (retired 24 Jul 2026)

LLM_LABELS_TIBBLE <- tibble::tribble(
    ~model_type, ~model, ~label, ~reasoning_effort,

    # # Cloud models: current mid-tier models
    # "cloud", "mistral-medium-latest", "Mistral Medium 3.5", NA_character_,
    # # Check the lowest reasoning_effort gpt-6-luna accepts with test-api
    # "cloud", "gpt-6-luna", "GPT-6 luna", "low",
    # "cloud", "gpt-6-luna", "GPT-6 luna", "medium",
    # "cloud", "gpt-6-luna", "GPT-6 luna", "high",
    # # Claude Sonnet 5.5: no "off" setting, as "between_tools" (no extended thinking) still uses the default effort (high)
    # "cloud", "claude-sonnet-5-5", "Claude Sonnet 5.5", "low",
    # "cloud", "claude-sonnet-5-5", "Claude Sonnet 5.5", "medium",
    # "cloud", "claude-sonnet-5-5", "Claude Sonnet 5.5", "high",
    # "cloud", "gemini-3.8-flash", "Gemini 3.8 flash", "low",
    # REPAIR: only the high run is repaired
    # "cloud", "gemini-3.8-flash", "Gemini 3.8 flash", "medium",
    "cloud", "gemini-3.8-flash", "Gemini 3.8 flash", "high",
    # "cloud", "deepseek-flash", "Deepseek flash", "off",
    # "cloud", "deepseek-flash", "Deepseek flash", "medium",
    # "cloud", "deepseek-flash", "Deepseek flash", "high",
    # "cloud", "qwen3.7-plus", "Qwen3.7-plus", "off",
    # "cloud", "qwen3.7-plus", "Qwen3.7-plus", "on",

    # Local models
    "local", "llama3.2", "Llama 3.2 (3.2B)", NA_character_,
    "local", "mistral", "Mistral (7.2B)", NA_character_,
    "local", "mistral-nemo", "Mistral nemo (12.2B)", NA_character_,
 
    NA_character_, "participants", "Humans", NA_character_
) %>%
    dplyr::mutate(
        label = dplyr::case_when(
            is.na(reasoning_effort) ~ label,
            reasoning_effort == "off" ~ stringr::str_c(label, " (no reasoning)"),
            reasoning_effort == "on" ~ stringr::str_c(label, " (reasoning)"),
            TRUE ~ stringr::str_c(label, " (", reasoning_effort, " reasoning)")
        )
    )

# Cloud runs: one row per model x reasoning effort; local models: model names
LLMS <- list(
    cloud = LLM_LABELS_TIBBLE %>%
        dplyr::filter(model_type == "cloud") %>%
        dplyr::select(model, reasoning_effort),
    local = LLM_LABELS_TIBBLE %>%
        dplyr::filter(model_type == "local") %>%
        dplyr::pull(model)
)



LLM_URLS <- c(
    gpt = "https://api.openai.com/v1/chat/completions",
    claude = "https://api.anthropic.com/v1/messages",
    `gemini-2.5-flash` = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-flash:generateContent",
    `gemini-2.5-pro` = "https://generativelanguage.googleapis.com/v1beta/models/gemini-2.5-pro:generateContent",
    `gemini-3.8-flash` = "https://generativelanguage.googleapis.com/v1beta/models/gemini-3.8-flash:generateContent",
    ollama = "http://localhost:11434/api/chat",
    `mistral` = "https://api.mistral.ai/v1/chat/completions",
    `deepseek` = "https://api.deepseek.com/chat/completions",
    `qwen-plus` = "https://dashscope-intl.aliyuncs.com/compatible-mode/v1/chat/completions"
    
)

# Due to our latin square design, we already have 12 (10 for exp 4) subjects
# We multiple this by the number below. for example, with N_SUBJ_LLM_AFTER_LSD = 10, we effectively have 120 or 100 subjects
# As the option order is counterbalanced (two Latin squares per condition), we use 5 replicates:
# 5 replicates x 12 Latin-square orders x 2 option orders = 120 simulated participants per condition.
# (Named differently from N_SUBJ_LLM_AFTER_LSD in moral_numbers_LLMs.Rmd, which is 10.)
N_SUBJ_LLM_AFTER_LSD_EXP3_RERUN <- 5


# Set to TRUE to run the simulations. Cloud models cost money (see llm_stuff/exp3_llm_rerun_cost_estimate.xlsx).
# Local models need ollama to be running.
RECALCULATE_LLM <- list(local = FALSE,
                        cloud = TRUE)

CHECKPOINT_DIR <- "checkpoints"


## ----combine-scenarios-numbers-------------------------------------------------------------



# dat.scenario.content %>% 
#     dplyr::filter(str_detect(experimentID, "exp10"))

l_lsd <- list(n10 = generate_latin_square(10),
                n12 = generate_latin_square(12))


# exp2b: ratio against vitims
# exp2ac: raiot again utility
# exp4

# 2. Cross scenarios × condition groups
# Experiment 3 only (exp2b = Exp. 3a, exp2ac = Exp. 3b)
dat_scenarios_numbers <- purrr::map(
    EXP3_IDS,
    combine_scenarios_and_number_conds
)


dat_scenarios_numbers <- purrr::map(
    dat_scenarios_numbers,
    \(dat){
        combo_names <- dat$combo_name
        
        purrr::pmap(list(dat$scenarios, dat$number_conds),
                    ~ apply_latin_design(..1, ..2, dat_lsd = l_lsd)) %>% 
            purrr::set_names(combo_names)
    }
)


dat_scenarios_numbers_flat <- dat_scenarios_numbers %>% 
    purrr::map(\(l){
        purrr::imap_dfr(l, ~ 
                            purrr::list_rbind(.x), .id = "combo_id") %>% 
            dplyr::mutate(
                dplyr::across(dplyr::starts_with("n_"), as.integer),
                # Options labelled A and B (see exp3-rerun-option-labels)
                prompt = make_prompt(text, question.acceptability, n_saved, n_victims, n_saved2, n_victims2, option1, option2,
                                     option_labels = c("A", "B"))) %>%
            combine_multiple_copies_of_columns 
    }) %>% 
    purrr::list_rbind() %>% 
    dplyr::rowwise() %>% 
    dplyr::mutate(subj = list(subj = glue::glue("{combo_id}_lsd{lsd_order}_s{1:N_SUBJ_LLM_AFTER_LSD_EXP3_RERUN}") %>%
                              as.character())) %>% 
    tidyr::unnest(subj) %>%
    # Split the option order off the condition (see exp3-rerun-counterbalance-option-order)
    tidyr::separate_wider_delim(condition, delim = ".", names = c("condition", "ratioOrder"))

# Safety check before any API call: only Experiment 3 trials
stopifnot(
    "Trials from experiments other than Experiment 3" =
        all(dat_scenarios_numbers_flat$experimentID %in% EXP3_IDS)
)

# Expected: 2 experiments x 2 conditions, each with 120 simulated participants x 12 trials
# (5,760 trials per model)
dat_llm_design_check <- dat_scenarios_numbers_flat %>%
    dplyr::count(experimentID, condition, name = "n_trials") %>%
    dplyr::left_join(
        dat_scenarios_numbers_flat %>%
            dplyr::distinct(experimentID, condition, subj) %>%
            dplyr::count(experimentID, condition, name = "n_subj"),
        by = c("experimentID", "condition")
    )

stopifnot(
    "Expected 4 cells (2 experiments x 2 conditions)" =
        nrow(dat_llm_design_check) == 4,
    "Expected 120 simulated participants per cell" =
        all(dat_llm_design_check$n_subj == 120),
    "Expected 1,440 trials per cell" =
        all(dat_llm_design_check$n_trials == 1440)
)

# Counterbalanced option order: half of the simulated participants per cell in each order
dat_llm_design_check_order <- dat_scenarios_numbers_flat %>%
    dplyr::distinct(experimentID, condition, ratioOrder, subj) %>%
    dplyr::count(experimentID, condition, ratioOrder, name = "n_subj")

stopifnot(
    "Expected 2 option orders per cell" =
        nrow(dat_llm_design_check_order) == 8,
    "Expected 60 simulated participants per cell and option order" =
        all(dat_llm_design_check_order$n_subj == 60)
)



## ----exp3-rerun-check-option-labels--------------------------------------------------------
# Check of the option labels in the composed prompts (see exp3-rerun-option-labels); nothing is replaced here
n_ab_option_labels <- function(prompt, label) sum(stringr::str_count(prompt, stringr::fixed(label)) == 1)

stopifnot(
    "Option numbers left in the prompts" =
        !any(stringr::str_detect(dat_scenarios_numbers_flat$prompt, "[Oo]ption [12]|\\([12]\\) ")),
    "Each prompt needs exactly one (A) and one (B)" =
        n_ab_option_labels(dat_scenarios_numbers_flat$prompt, " (A) ") == nrow(dat_scenarios_numbers_flat) &&
            n_ab_option_labels(dat_scenarios_numbers_flat$prompt, " (B) ") == nrow(dat_scenarios_numbers_flat)
)



## ----test-api, eval = FALSE----------------------------------------------------------------
# 
# # Costs a few cents in total. Sends one request per model x reasoning effort; check that each returns a
# # single digit and that no request fails.
# purrr::pmap_dfr(
#     LLMS$cloud,
#     \(model, reasoning_effort){
# 
#     m <- model
#     url <- get_model_url(m)
# 
#     message <- tibble::tribble(
#         ~role, ~message,
#         #"user", "what is the capital of england: 1 = bristol, 2 = leeds, 3 = london, 4 = paris, 5=hawai?") %>%
#         "user", "A bat and a ball cost 1.10 in total. The bat costs 1.00 more than the ball. How much does a lay person think the the ball is in cents? Subtract this from how much it actually costs. Answer with the number only.") %>%
#         compose_llm_messages(model = m)
# 
#     # The system prompt is passed in the API call for gemini and claude, and as a system message otherwise
#     if (!stringr::str_detect(m, "gemini|claude")) {
#         message <- c(
#             tibble::tribble(
#                 ~role, ~message,
#                 "system", "just answer with numbers, anything else makes your output unusable.") %>%
#                 compose_llm_messages(model = m),
#             message
#         )
#     }
# 
#     response <- tryCatch(
#         send_llm_request(
#             model = m,
#             url = url,
#             messages = message,
#             system_prompt = "just answer with numbers, anything else makes your output unusable.",
#             system_instruction = list(parts = list(list(text = "just answer with numbers, anything else makes your output unusable."))),
#             DEBUG = TRUE,
#             reasoning_effort = reasoning_effort,
#             body_fn = make_llm_body_reasoning,
#             keep_reasoning = TRUE
#         ),
#         error = function(e) {
#             message("Error in send_llm_request: ", e$message)
#             list(answer = NA_character_, reasoning = e$message)
#         }
#     )
# 
#     data.frame(
#         model = m,
#         reasoning_effort = reasoning_effort,
#         # send_llm_request() returns the uncleaned answer
#         response_raw = response$answer,
#         reasoning = response$reasoning
#     )
# }
# ) -> llm_test_responses
# 
# 
# 


## ----inspect-request-bodies, eval = FALSE--------------------------------------------------
# # Free: builds the request body of the first trial of the first Exp. 3a participant for each
# # model x reasoning effort without sending anything
# 
# dat_first_trial <- dat_scenarios_numbers_flat %>%
#     dplyr::filter(subj == dplyr::first(subj)) %>%
#     dplyr::slice(1)
# 
# purrr::pwalk(
#     dplyr::bind_rows(
#         LLMS$cloud,
#         tibble::tibble(model = LLMS$local, reasoning_effort = NA_character_)
#     ),
#     \(model, reasoning_effort){
#         m <- model
#         messages <- tibble::tribble(
#             ~role, ~message,
#             "user", dat_first_trial$prompt) %>%
#             compose_llm_messages(model = m)
# 
#         body <- make_llm_body_reasoning(
#             m,
#             messages,
#             system_prompt = SYSTEM_PROMPTS$comparison,
#             system_instruction = SYSTEM_INSTRUCTIONS$comparison,
#             reasoning_effort = reasoning_effort
#         )
#         # Don't print the long prompts
#         body[intersect(names(body), c("messages", "contents", "system", "systemInstruction"))] <- "<...>"
# 
#         cat("\n=====", m, "/", reasoning_effort, "->", get_model_url(m), "=====\n")
#         print(jsonlite::toJSON(body, auto_unbox = TRUE, pretty = TRUE))
#     }
# )


## ----count-tokens, eval = FALSE------------------------------------------------------------
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


## ----LLM-option-augment--------------------------------------------------------------------

llm_option_metadata <- list(
    max_attempts = list(
        description  = "Max. API call retries on failure",
        available_in = "All models (client-side)"
    ),
    max_output_tokens = list(
        description  = "Max. tokens in model response",
        available_in = "GPT-4, Claude, Mistral, Deepseek, Qwen"
    ),
    max_output_tokens_reasoning = list(
        description  = "Max. tokens in model response incl. reasoning (reasoning switched on)",
        available_in = "Claude Sonnet 5.5, Deepseek flash, Qwen3.7-plus"
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




## ----LLM-option-print----------------------------------------------------------------------


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



## ----LLM-system-prompt-print---------------------------------------------------------------


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
    

    


## ----number-of-trials-print----------------------------------------------------------------

dat_llm_n_trials_responses <- purrr::reduce(
    list(
   
        # Cells in latin square (one Latin square per option order)
        dat_scenarios_numbers_flat %>%
            dplyr::distinct(experimentID, condition, Group, ratioOrder, lsd_order)  %>%
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
    dplyr::mutate("Number of participants" = N_SUBJ_LLM_AFTER_LSD_EXP3_RERUN * `Number of cells`,
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
    



## ----run-exp-------------------------------------------------------------------------------

# All results are saved to new files starting with OUTPUT_FILE_PREFIX, so the original
# llm_test.all_exps.*.RData files can never be overwritten
# File name: <prefix>.<model>[.<reasoning effort>].RData
make_llm_output_file_name <- function(model, reasoning_effort = NA_character_){
    run_name <- if (is.na(reasoning_effort)) model else glue::glue("{model}.{reasoning_effort}")
    # OUTPUT_FILE_PREFIX is referenced outside of glue() so that the parallel workers (furrr) receive it
    file_name <- stringr::str_c(OUTPUT_FILE_PREFIX, ".", run_name, ".RData")
    stopifnot(
        "Output file name must start with OUTPUT_FILE_PREFIX" =
            stringr::str_starts(file_name, stringr::fixed("llm_exp3_fixed_prompt.")),
        "Model and reasoning effort must not contain a path" =
            !stringr::str_detect(run_name, "/")
    )
    here::here(OUTPUT_DIR, file_name)
}

# REPAIR: rerun only the conversations of the saved run that failed (e.g., 429 errors after the daily request
# limit) or are incomplete, and merge them with the valid conversations of the saved run (in the cloud loop below)
REPAIR_MODEL <- "gemini-3.8-flash"
REPAIR_EFFORT <- "high"
REPAIR_FILE <- make_llm_output_file_name(REPAIR_MODEL, REPAIR_EFFORT)

stopifnot(
    "Only the run to be repaired may be active" =
        nrow(LLMS$cloud) == 1 &&
        LLMS$cloud$model == REPAIR_MODEL &&
        LLMS$cloud$reasoning_effort %in% REPAIR_EFFORT,
    "The results to be repaired do not exist" =
        file.exists(REPAIR_FILE)
)

# REPAIR (1): load the saved results
dat_repair_saved <- load_file_as_value(REPAIR_FILE)

if (!"error" %in% names(dat_repair_saved)) {
    dat_repair_saved <- dat_repair_saved %>%
        dplyr::mutate(error = NA_character_)
}

# REPAIR (2): conversations to rerun: with an error, incomplete, or missing from the saved results
dat_repair_n_trials <- dat_scenarios_numbers_flat %>%
    dplyr::count(subj, name = "n_design") %>%
    dplyr::left_join(
        dat_repair_saved %>%
            dplyr::group_by(subj) %>%
            dplyr::summarize(
                n_saved = dplyr::n(),
                has_error = any(!is.na(error))
            ),
        by = "subj"
    )

subj_repair <- dat_repair_n_trials %>%
    dplyr::filter(
        is.na(n_saved) | n_saved != n_design | has_error
    ) %>%
    dplyr::pull(subj)

if (length(subj_repair) == 0) {
    stop("No conversations to rerun in ", basename(REPAIR_FILE), ".")
}

message(length(subj_repair), " of ", nrow(dat_repair_n_trials), " conversations to rerun:")
dat_scenarios_numbers_flat %>%
    dplyr::filter(subj %in% subj_repair) %>%
    dplyr::distinct(experimentID, condition, ratioOrder, subj) %>%
    dplyr::count(experimentID, condition, ratioOrder) %>%
    as.data.frame() %>%
    print()

# REPAIR (3): valid conversations of the saved results
dat_repair_valid <- dat_repair_saved %>%
    dplyr::filter(!subj %in% subj_repair)

# Keep a copy of the saved results, as the merged results are saved under the same file name
if (isTRUE(RECALCULATE_LLM$cloud)) {
    stopifnot(
        file.copy(
            REPAIR_FILE,
            paste0(REPAIR_FILE, ".", format(Sys.time(), "%Y%m%d_%H%M%S"))
        )
    )
}

# Only the conversations to be rerun (all trials) are simulated
dat_scenarios_numbers_flat_full <- dat_scenarios_numbers_flat
dat_scenarios_numbers_flat <- dat_scenarios_numbers_flat %>%
    dplyr::filter(subj %in% subj_repair)

# Safety check before any API call: only Experiment 3 trials
stopifnot(
    "Trials from experiments other than Experiment 3" =
        all(dat_scenarios_numbers_flat$experimentID %in% EXP3_IDS)
)


if(isTRUE(RECALCULATE_LLM$local)){

    # In the original simulations, llama3.2 took 684 s/11.4 min per subject when run through Rscript.

    dat_llm_results_exps_local <- dat_scenarios_numbers_flat %>%
        tidyr::expand_grid(model = LLMS$local) %>%
        dplyr::group_by(model, experimentID, experimentDesc, condition, Group, lsd_order, combo_id, subj) %>%
        dplyr::group_modify(
            # dat: the trials of one simulated participant and model; dat_grp: the grouping variables
            \(dat, dat_grp) get_api_response_wrapper_df(
                dat,
                dat_grp,
                verbose = TRUE,
                DEBUG = FALSE,
                body_fn = make_llm_body_reasoning,
                keep_raw_response = TRUE
            ),
            .keep = FALSE
        ) %>%
        tidyr::unnest(response) %>%
        dplyr::mutate(response = as.numeric(response)) %>%
        dplyr::rename(rating.raw = response) %>%
        dplyr::rename_with(~ stringr::str_replace_all(.x, "_", "."), dplyr::matches("n_"))  %>%
        # Invalid answers are kept in the saved file (see response_raw); they are removed in combine-llm-results
        dplyr::mutate(dplyr::across(dplyr::all_of(c("n.victims", "n.saved")), as.integer),
                      rating.bin = 1 * (rating.raw > 3),
                      n.net.saved =  n.saved - n.victims,
                      n.total = n.victims + n.saved)

    save(
        dat_llm_results_exps_local,
        file = make_llm_output_file_name("local_models")
        )
} else if (file.exists(make_llm_output_file_name("local_models"))) {
    load(make_llm_output_file_name("local_models"))
} else {
    message("No results for local models yet.")
    dat_llm_results_exps_local <- tibble::tibble()
}


if(isTRUE(RECALCULATE_LLM$cloud)){

    # Parellelize only by run (model x reasoning effort)
    # We save run specific RData files from within the loop
    # A failed request stops the conversation (stop_on_error = TRUE); the error is recorded in the error column

    safe_get_api_response <- purrr::safely(get_api_response_wrapper_df, otherwise = NULL)

    l_cloud_result_vars <- furrr::future_pmap_chr(
        LLMS$cloud,
        \(model, reasoning_effort){
            Sys.sleep(runif(1, 0.1, 0.5))  # jittered sleep

            partial_result <- dat_scenarios_numbers_flat %>%
                dplyr::mutate(model = model,
                              reasoning_effort = reasoning_effort) %>%
                dplyr::group_by(model, reasoning_effort, experimentID, experimentDesc, condition, Group, lsd_order, combo_id, subj) %>%
                dplyr::group_modify(
                    \(dat, dat_grp){

                        res <- safe_get_api_response(dat, dat_grp, verbose = TRUE, DEBUG = FALSE,
                                                     body_fn = make_llm_body_reasoning,
                                                     keep_reasoning = TRUE,
                                                     stop_on_error = TRUE,
                                                     keep_raw_response = TRUE)

                        if (!is.null(res$error)) {
                            tibble::tibble(
                                error = paste("Error:", res$error$message)
                            )
                        } else {
                            res$result
                        }
                    },
                    .keep = FALSE)

            # REPAIR (4): merge the rerun conversations with the valid conversations of the saved results,
            # sorted and grouped as in the original results (the sort is stable, so the trial order is kept)
            partial_result <- dplyr::bind_rows(
                dat_repair_valid,
                dplyr::ungroup(partial_result)
            ) %>%
                dplyr::arrange(model, reasoning_effort, experimentID, experimentDesc, condition, Group, lsd_order, combo_id, subj) %>%
                dplyr::group_by(model, reasoning_effort, experimentID, experimentDesc, condition, Group, lsd_order, combo_id, subj)

            if (!"error" %in% names(partial_result)) {
                partial_result <- partial_result %>%
                    dplyr::mutate(error = NA_character_)
            }

            # Each conversation of the design must be present; without an error, with all trials
            # (conversations that failed again have an error row and can be repaired by running this script again)
            dat_repair_check <- dat_scenarios_numbers_flat_full %>%
                dplyr::count(subj, name = "n_design") %>%
                dplyr::full_join(
                    partial_result %>%
                        dplyr::ungroup() %>%
                        dplyr::group_by(subj) %>%
                        dplyr::summarize(
                            n_merged = dplyr::n(),
                            has_error = any(!is.na(error))
                        ),
                    by = "subj"
                )

            stopifnot(
                "Conversations of the design are missing from the merged results" =
                    !any(is.na(dat_repair_check$n_merged)),
                "The merged results contain conversations that are not in the design" =
                    !any(is.na(dat_repair_check$n_design)),
                "Conversations without an error must have all trials" =
                    with(dat_repair_check, all(has_error | n_merged == n_design))
            )

            message("Conversations with errors after the repair: ", sum(dat_repair_check$has_error))

            # As in runs without failed conversations
            if (all(is.na(partial_result$error))) {
                partial_result <- partial_result %>%
                    dplyr::select(-error)
            }

            # Save run specific data file
            # OUTPUT_FILE_PREFIX is referenced outside of glue() so that the parallel workers (furrr) receive it
            # paste0() rather than stringr::str_c(), as the reasoning effort can be NA
            result_var_name <- paste0("dat_", OUTPUT_FILE_PREFIX, "_", gsub("[-.]", "_", model), "_", reasoning_effort)
            # Don't care in which environment it shows up as it will be saved anyhow
            assign(result_var_name,
                   partial_result)

            save(list = result_var_name,
                 file = make_llm_output_file_name(model, reasoning_effort)
            )

            result_var_name
        },
        .options = furrr::furrr_options(seed = TRUE)
    )

}

# Load the run specific data files (also after running the simulations, as these run in separate processes)
l_cloud_result_vars <-
    purrr::pmap(
        LLMS$cloud,
        \(model, reasoning_effort){

            file <- make_llm_output_file_name(model, reasoning_effort)

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



## ----save-exp3-rerun-metadata--------------------------------------------------------------
# Metadata needed by moral_numbers_LLMs.Rmd to include the rerun in its analyses: models, labels, prompts,
# options and the names of the result files (without directory). The result files themselves are loaded one
# by one by moral_numbers_LLMs.Rmd.
llm_exp3_rerun_metadata <- list(
    exp_ids = EXP3_IDS,
    system_prompts = SYSTEM_PROMPTS,
    llm_options = LLM_OPTIONS,
    labels = LLM_LABELS_TIBBLE,
    cloud_runs = LLMS$cloud %>%
        dplyr::mutate(file = purrr::map2_chr(model, reasoning_effort, ~ basename(make_llm_output_file_name(.x, .y)))),
    local_models = LLMS$local,
    local_file = basename(make_llm_output_file_name("local_models")),
    # Design (one row per trial, without prompts), for the design table in moral_numbers_LLMs.Rmd
    design = dat_scenarios_numbers_flat %>%
        dplyr::select(experimentID, experimentDesc, condition, ratioOrder, Group, lsd_order, combo_id, subj)
)

# REPAIR: the metadata are not saved, as they would only list the repaired run
# save(
#     llm_exp3_rerun_metadata,
#     file = make_llm_output_file_name("metadata")
# )


## ----combine-llm-results-------------------------------------------------------------------

dat_llm_results_exps_cloud <- purrr::map(l_cloud_result_vars,
    ~ get(.x, envir = .GlobalEnv) %>%
        dplyr::ungroup()
) %>%
    purrr::list_rbind()

if (NROW(dat_llm_results_exps_cloud) > 0) {
    dat_llm_results_exps_cloud <- dat_llm_results_exps_cloud %>%
        # Forgot to add this to saved files for individual models
        tidyr::unnest(response, keep_empty = TRUE) %>%
        dplyr::mutate(response = as.numeric(response)) %>%
        dplyr::rename(rating.raw = response) %>%
        dplyr::rename_with(~ stringr::str_replace_all(.x, "_", "."), dplyr::matches("n_"))  %>%
        tidyr::drop_na(rating.raw) %>%
        dplyr::mutate(dplyr::across(dplyr::all_of(c("n.victims", "n.saved")), as.integer),
                      rating.bin = 1 * (rating.raw > 3),
                      n.net.saved =  n.saved - n.victims,
                      n.total = n.victims + n.saved)
}

# Rbind results
dat_llm_results_exps <- dplyr::bind_rows(
    dat_llm_results_exps_local %>%
        dplyr::ungroup(),
    dat_llm_results_exps_cloud
)

# Valid responses are ratings from 1 to 6 (for local and cloud models alike)
if (nrow(dat_llm_results_exps) > 0) {
    dat_llm_results_exps <- dat_llm_results_exps %>%
        dplyr::filter(rating.raw %in% 1:6)
}

# The local models have no reasoning effort; the column is missing if there are no cloud results
if (!"reasoning_effort" %in% names(dat_llm_results_exps)) {
    dat_llm_results_exps <- dat_llm_results_exps %>%
        dplyr::mutate(reasoning_effort = NA_character_)
}

if (nrow(dat_llm_results_exps) == 0) {
    # The rest of the document is skipped
    knitr::knit_exit("No results yet: set RECALCULATE_LLM$local and/or RECALCULATE_LLM$cloud to TRUE to run the simulations.")
} else {
    dat_llm_results_exps <- dat_llm_results_exps %>%
        # For compatibility with the human experiments
        dplyr::rename(ResponseId = subj)
}


## ----invert-rartings-exp2-llm--------------------------------------------------------------

# In the human experiments, 6 means the high ratio option is code as the second option (though the option order is counterbalanced, here it's the first)
# The option order is counterbalanced (ratioOrder): ratings are inverted only when the high-ratio option was listed
# first (option A); when it was listed second (option B), 6 already means the high-ratio option.

dat_llm_results_exps <- dat_llm_results_exps %>%
    dplyr::mutate(invert = stringr::str_detect(experimentID, "exp2") & ratioOrder == "high_ratio_first",
                  rating.bin = dplyr::if_else(invert,
                                               1 - rating.bin, rating.bin),
                  rating.raw = dplyr::if_else(invert,
                                               7 - rating.raw, rating.raw)
    ) %>%
    dplyr::select(-invert)



