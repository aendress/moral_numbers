# Helper Functions

All `.R` files in this directory are sourced automatically at the start of `moral_numbers.Rmd`. Each file groups functions by theme.

---

## moral_numbers_general.R

General-purpose data manipulation and formatting utilities used throughout the document.

| Function | Description |
|---|---|
| `get.question.labels.from.qsf` | Extracts question labels and IDs from a Qualtrics `.qsf` export file |
| `get.number.ratio` | Formats a numeric ratio as a human-readable string (e.g. `2` → `"2:1"`) |
| `separate.multiple.field.value.pairs` | Splits combined field-value pair strings into separate columns |
| `scenarios_to_long` | Reshapes wide scenario data into long format |
| `calculate_number_conds` | Computes the number conditions (victims, saved, ratio) for each experiment |
| `split_condition_name` | Parses experiment condition labels into constituent parts |
| `convert.dates.and.calculate.duration` | Converts Qualtrics timestamps and computes survey completion duration |
| `parse_free_text_number` | Converts free-text numeric responses (e.g. age) to numbers: collapses whitespace and extracts the first number (`" 35 "`, `"35 years"` → `35`) |
| `L_EXP_ID_SUFFIXES` | Registry of experiment-ID suffixes marking pseudo-experiments, in their required order: `<expID_clean>[.scenarioExcluded][.vivacityManipulationWorking]` |
| `parse_exp_id` | Internal helper: splits experiment IDs into the clean ID and suffix flags (`scenarios_excluded`, `vivacity_filtered`); stops on unknown suffixes or wrong order |
| `make_exp_id` | Builds canonical experiment IDs from a clean ID and suffix flags (inverse of `parse_exp_id`); use wherever pseudo-experiment IDs are created |
| `select_exps` | Selects experiments/pseudo-experiments from a data frame (rows) or a vector of IDs, by clean ID and suffix flags (TRUE/FALSE/NA); `scenarios_excluded = TRUE` uses the scenario-excluded version where one exists (`l_exp_with_scenario_exclusions`, or a scenario-excluded pooled experiment in the data); `require_single_version = TRUE` stops unless exactly one version per experiment remains |
| `pool_exps` | Pools several experiments into one combined ID (e.g. `exp8+10`), separately for each version: original versions, scenario-excluded versions (excluded version where it exists, original otherwise), and vivacity-filtered/unfiltered; returns only the pooled rows |
| `get_native_english_share` | Percentage of participants (per grouping) who list English among their free-text native language(s); missing responses are excluded |
| `summarize.first.by.subj.then.by.overall` | Two-stage summarisation: first within participant, then across participants |
| `get.rating.mean.se` | Computes mean and standard error of ratings |
| `define_vivacity_col` | Creates/normalises the vivacity condition column |
| `define_question_col` | Creates/normalises the question type column |
| `make.Z.value.against.control` | Computes a Z value for a parameter estimate relative to a control distribution |
| `add_z_relative_to_baseline` | Adds a Z score relative to a baseline bootstrap distribution |
| `rename.terms` | Renames model terms to human-readable labels for display |
| `combine_exps` | Merges data from multiple experiments into a single data frame |
| `get.original.experimentID` | Maps internal experiment IDs to the original experiment labels |
| `format_exp_info` | Formats experiment ID strings for display in tables and figures; shows `.vivacityManipulationWorking` as "(filtered)" and drops `.scenarioExcluded` unless `show_scenario_exclusion = TRUE` |
| `filter_vivacity_exps` | Filters the dataset to experiments with a vivacity manipulation (all versions, matched on the clean experiment ID) |
| `summarize.ratings` | Summarises raw and binary ratings (mean, SE, p-value, effect size) across participants |

---

## moral_numbers_models.R

Psychophysical model functions: the core Weber-ratio acceptability model, bootstrap fitting, and fit-summary utilities.

| Function | Description |
|---|---|
| `discriminability.fnc` | Computes discriminability (d') between two quantities under Weber's law |
| `acceptability.fnc` | Core model: predicted probability of a sacrificial decision being acceptable given w, α, and ratio |
| `generate_bootstrap_samples` | Draws bootstrap samples of participants (with replacement); `verbose` shows the output of the parallel workers |
| `bootstrap.weber.ratio` | Fits the one-parameter (w) model to each bootstrap sample |
| `bootstrap.weber.ratio.new` | Updated version of bootstrap w fitting with additional output |
| `get.by.subj.weber.ratio` | Estimates w separately for each participant |
| `get.weber.ratio` | Fits w to group-averaged data |
| `get.weber.ratio.linearized` | Fits w using a linearised formulation |
| `get.weber.ratio.with.prediction.error` | Fits w and returns prediction error alongside the estimate |
| `get.threshold` | Finds the ratio threshold at which predicted acceptability crosses a criterion |
| `get.estimates.from.bootstrap.fit` | Extracts w estimate and bootstrap percentile interval as a formatted string |
| `describe_fit_params` | Formats NLS fitting parameter values (start, lower, upper bounds) for inline reporting |

---

## moral_numbers_plots.R

Plotting functions for model fits, bootstrap summaries, and vivacity experiment figures.

| Function | Description |
|---|---|
| `create.fit.plot` | Creates a plot of model fit overlaid on observed acceptability data |
| `get_dat_fit_for_plot` | Prepares a data frame of model predictions for plotting |
| `bootstrap.summary.to.fit.summary` | Converts bootstrap sample summaries to a fit-summary data frame for plotting |
| `create.predictor.comparison.plot` | Plots acceptability as a function of multiple candidate predictors (ratio, utility, harm) |
| `combine_vivacity_detailed_plots` | Assembles a 2×2 panel of vivacity condition plots (binarized/raw × all blocks/first block) for a single experiment |
| `arrange_with_legends_below` | Arranges panels (labelled a, b, ...) with each selected panel's legend underneath its plot, aligning plot areas across panels (used for the model illustration figure) |

---

## moral_numbers_model_comparisons.R

Utilities for comparing nested mixed-effects models.

| Function | Description |
|---|---|
| `get.dv.from.model` | Extracts the dependent variable vector from a fitted `lmer`/`glmer` model |
| `j.test.glmer` | Experimental J-test for non-nested GLMM comparison (not recommended for use) |
| `generate_model_comp` | Runs a set of model comparisons and returns a formatted results table |

---

## moral_numbers_relative_risk.R

Functions for computing and plotting relative risk (RR) of acceptable sacrificial decisions in vivid vs. neutral conditions.

| Function | Description |
|---|---|
| `create_acceptability_contingency_table` | Builds a contingency table of acceptable/unacceptable responses by vivacity condition |
| `make_rr_neutral_vivid` | Computes the relative risk RR = P(acceptable, neutral) / P(acceptable, vivid) for each ratio |
| `create_rr_plot` | Plots RR as a function of the beneficiary:victim ratio |

---

## moral_numbers_bootstrap_hesterberg.R

Bootstrap confidence interval methods following Hesterberg (2015).

| Function | Description |
|---|---|
| `boot.ti` | Computes bootstrap-t percentile intervals |
| `summarize_bootstrap_samples` | Summarises bootstrap distributions into point estimates and percentile intervals |

---

## moral_numbers_prospect.R

Functions for the prospect theory / loss aversion analyses (comparison to Tversky & Kahneman 1992).

| Function | Description |
|---|---|
| `create_data_for_prospect_demo` | Generates model predictions of subjective value as a function of relative gain/loss, for a given endowment and set of w values |
| `create_prospect_change_value_plot` | Plots the S-shaped subjective value curve (utility vs. relative change) |
| `create_prospect_relative_value_plot` | Plots the loss aversion ratio (|utility_loss| / |utility_gain|) as a function of relative change |
| `add_tk_data` | Adds Tversky & Kahneman (1992) model predictions to a prospect demo data frame |

---

## moral_numbers_asymptote.R

Functions for algorithmically identifying the asymptotic ratio range in vivacity experiments (Experiments 4–6).

| Function | Description |
|---|---|
| `get.ratio.before.asymptote` | Identifies the largest ratio below the asymptote using paired Wilcoxon or McNemar tests against the rightmost ratio |
| `add_experimentID_to_cond_find_asympt` | Expands a condition data frame by replicating it for each of a set of experiment IDs |
| `get.ratio.before.asymptote.bootstrap` | Applies `get.ratio.before.asymptote` to a single bootstrap sample across all conditions |
| `run_bootstrap_fit_for_asymptote` | Runs the full bootstrap asymptote-identification procedure in batches, with parallelisation via `furrr`; `verbose` prints the search progress |

---

## moral_numbers_parameter_recovery.R

Simulation functions for assessing parameter recovery of w and α.

| Function | Description |
|---|---|
| `rnorm_mixture` | Samples from a Gaussian mixture model with specified component means, SDs, and mixing weights |
| `fit_with_nlsLM` | Wraps `minpack.lm::nlsLM` with error handling, returning a tidy data frame of estimates |

---

## moral_numbers_power.R

Empirical power analysis functions for detecting the interaction between ratio range (pre-asymptotic vs. asymptotic) and vivacity condition.

| Function | Description |
|---|---|
| `generate_dummy_data` | Generates synthetic binary trial data from four probabilities (neutral/vivid × pre-asymptotic/asymptotic) |
| `simulate_and_check_significance_ia` | Runs one simulation and tests the ratio range × vivacity interaction via a GLMM |
| `simulate_and_check_significance_wrapper` | Runs `n_sim` simulations across a data frame of parameter conditions and returns empirical power estimates |
| `describe_ratio_range_params` | Formats a parameter-range specification data frame as a prose sentence for inline reporting |
| `simulate_and_check_significance_diff_score` | Runs one simulation and tests the ratio range effect on vivid–neutral difference scores via a paired Wilcoxon test |

---

## moral_numbers_LLM_utilities.R

Label and classification utilities for LLM data: model type assignment, display-name mapping, and condition/model-type factor creation.

| Function | Description |
|---|---|
| `classify_models` | Adds a `model_type` column (`"cloud"` / `"local"`) based on `LLM_LABELS_TIBBLE` |
| `relabel_models` | Maps internal model identifiers to display labels via `LLM_LABELS`, returning a factor with Humans first then alphabetically sorted models |
| `relabel_conds_llm` | Converts condition strings to title case and returns a factor with levels `Moral`, `Economic`, `Neutral`, `Vivid` |
| `relabel_model_type` | Recodes `model_type` values (`"cloud"` → `"Frontier models"`, `"local"` → `"Local models"`, `NA` → `"Humans"`) and returns a factor; works on both data frames and vectors |

---

## moral_numbers_xml.R

Utilities for parsing XML scenario files into tidy data frames.

| Function | Description |
|---|---|
| `xml_to_df` | Converts an XML document into a data frame with one row per target node (default: `<scenario>`), extracting all child node values as columns |

---

## moral_numbers_design.R

Experimental design utilities: Latin square generation, scenario/number-condition combination, and duplicate column resolution.

| Function | Description |
|---|---|
| `generate_latin_square` | Generates an n×n Latin square design using `agricolae::design.lsd`, optionally returning a matrix |
| `apply_latin_design` | Applies a Latin square ordering to two data frames (`dat1`, `dat2`), returning a list of combined data frames — one per column of the Latin square |
| `combine_scenarios_and_number_conds` | Combines scenario content and number conditions for a given experiment ID, splitting by condition and group as needed, and returns all scenario × number-condition combinations |
| `combine_multiple_copies_of_columns` | Collapses duplicate columns sharing a base name (e.g., `experimentID`, `experimentID...2`) into a single column, erroring on conflicting non-NA values within a row |

---

## moral_numbers_LLM_API_functions.R

Low-level API utilities for querying frontier and local LLMs: URL dispatch, header/body construction, message formatting, and response parsing.

| Function | Description |
|---|---|
| `get_model_url` | Returns the API endpoint URL for a given model name, dispatching by provider (OpenAI, Anthropic, Google, Mistral, DeepSeek, Qwen, Ollama); models in `LLMS$local` always go to Ollama |
| `make_llm_header` | Builds the HTTP authentication headers for a given model's API |
| `make_llm_body` | Constructs the JSON request body for a given model, encoding messages, temperature, token limits, and provider-specific parameters |
| `make_llm_body_reasoning` | Request bodies for the models of the Experiment 3 rerun (GPT-6 luna, Claude Sonnet 5.5, Gemini 3.8 Flash, deepseek-flash, Qwen3.7-plus, local models); takes the reasoning effort as fifth argument (`body_fn` of `send_llm_request`); other models get `make_llm_body()` |
| `parse_llm_response` | Extracts the text content from an API response object, dispatching by provider (Claude: text blocks by type; Gemini: thought parts skipped); with `keep_reasoning = TRUE` returns `list(answer, reasoning)` (Claude thinking summaries, Gemini thoughts, DeepSeek/Qwen `reasoning_content`; `NA` for OpenAI) |
| `send_llm_request` | Sends a POST request to an LLM API with retry logic (including 429 rate-limit backoff) and returns the parsed text response; stops with the provider's message on any non-200 status. Optional `reasoning_effort` and `body_fn` (default `make_llm_body`; called with the effort as fifth argument when it is not `NA`) and `keep_reasoning` |
| `compose_llm_messages` | Converts a data frame of `role`/`message` pairs into the provider-specific message list format required by the API |
| `get_api_response_incremental` | Builds a multi-turn message list from a conversation data frame (previous answers only, no reasoning) and calls `send_llm_request`; handles different system prompt conventions per provider; passes `reasoning_effort`, `body_fn`, `keep_reasoning` through; `clean_answer = TRUE` (default) keeps only the leading digits of the answer |
| `get_api_response_wrapper_df` | Wrapper for `get_api_response_incremental` that iterates over rows of a grouped data frame, filling in responses one by one with error handling; takes the reasoning effort from `dat_grp$reasoning_effort` if present; `keep_reasoning = TRUE` adds a `reasoning` column; `stop_on_error = TRUE` stops at the first failed request instead of recording `NA`; cleans the answers itself (leading digits); `keep_raw_response = TRUE` adds a `response_raw` column with the uncleaned answers |
| `make_prompt` | Constructs a trial prompt string by substituting `n_saved`, `n_victims`, etc. into a template, with special handling for two-option (Exp. 2) prompts; `option_labels` sets the labels of the two options (default `c("1", "2")`) |

---

## moral_numbers_LLMs_plots.R

Plotting functions for the LLM comparison analyses: model fit plots, predictor comparison panels, and the 2D joint preference scatter plot.

| Function | Description |
|---|---|
| `get_dat_fit_for_plot_llm` | LLM-specific version of `get_dat_fit_for_plot`; extracts w/α estimates from `dat_llm_fits_1_param` or `dat_llm_fits_2_param` and returns a wide-format data frame for use with `create.fit.plot.llm` |
| `create.fit.plot.llm` | LLM-specific version of `create.fit.plot`; plots observed acceptability with model fit curves, supporting group and facet variables with `droplevels()`-based level handling to avoid phantom factor warnings |
| `create.predictor.comparison.plot.llm` | LLM-specific version of `create.predictor.comparison.plot`; assembles a 2×2 panel of acceptability by ratio, proportion saved, number of victims, and utility, with optional model fit overlay |
| `prepare_llm_internal_exp2_plot_data` | Prepares the plot data for the LLM version of internal exp2 (paper Exp. 3a/3b) for a single version (`scenarios_excluded` passed to `select_exps()`): experiment labels, preference categories, 95% CIs and model order; returns `list(long, wide_by_experiment, wide_by_condition)`: `wide_by_experiment` (one row per model and condition, columns per experiment) is used by `make_llm_exp2_joint_plot`, `wide_by_condition` (one row per model and experiment, columns per condition) by `make_llm_exp3_moral_economic_plot` |
| `make_llm_exp2_joint_plot` | Creates the 2D joint preference scatter plot (Exp. 3a vs. Exp. 3b ratings) from the `wide_by_experiment` data of `prepare_llm_internal_exp2_plot_data` (`dat`), with per-condition quadrant frames (border-only, in human colour), quadrant labels, and optional ggrepel text labels; parameterised by `base_size`, `label_size`, `add_repel`, and `repel_size` |
| `make_llm_exp3_moral_economic_plot` | Scatter plot of the preference for the high-ratio option in moral (y) vs. economic (x) dilemmas, one point per model and one panel per experiment (Exp. 3a, 3b), from the `wide_by_condition` data of `prepare_llm_internal_exp2_plot_data`; diagonal (no moral/economic difference), quadrant labels naming the preferred option, frame around the humans' quadrant; optional `experiments` filter, `facet_row` (column for rows of panels, e.g. the option order) and ggrepel labels (`base_size`, `label_size`, `add_repel`, `repel_size`); `quadrant_label_colour` and `human_frame_colour` (e.g., lighter colours on a black background) |
