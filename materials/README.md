# Experiment Materials

## Overview

This folder contains the stimulus materials for all experiments reported in the paper. There are two types of files:

**Scenario files** (`scenarios/`, XML format) describe the moral dilemmas presented to participants. Each file contains a series of `<scenario>` elements. The XML files were processed together with the corresponding number-conditions CSV files using custom Perl scripts: the numerical placeholders (`n_victims`, `n_saved`, `n_victims1`, etc.) were replaced with the values from the CSV, and the resulting scenarios were converted to Aiken format for upload to Qualtrics.

There are three structural variants across experiments:

- **Rating format** (Experiments 1a, 1b, 2): Each `<scenario>` has `<label>`, `<title>`, `<text>`, `<question>`, and `<nchoices>` (always 6). Participants rated the moral acceptability of sacrificing `n_victims` people to save `n_saved` people on the 6-point scale.

- **Forced-choice format** (Experiment 3): Each `<scenario>` has `<label>`, `<title>`, `<text>`, `<option1>`, `<option2>`, and `<question>`. Participants chose between two options, each defined by its own `n_victims1`/`n_saved1` and `n_victims2`/`n_saved2` placeholders.

- **Incommensurate-scenario format** (Experiments 4–6 and AS): Each `<scenario>` has paired vivid and neutral versions (distinguished by `.vivid` / `.neutral` suffixes on `<label>`). In addition to `<question.acceptability>` (the standard rating), there is a severity-comparison question (`<question.severity.text>`, `<question.severity.anchor.left>`, `<question.severity.anchor.right>`) asking which type of death participants found more horrific.

For Experiments 2 and 3, separate files exist for the moral and economic versions of the task (and, for Experiment 3, for the evil-choice conditions).

**Number condition files** (`number_conditions/`, CSV format) specify the numerical values (number of people saved, number of people harmed, total group size) that were substituted into the scenario placeholders for each trial. Each row is one trial. For experiments with counterbalancing groups (Experiments 4–6 and AS), there is one file per group; the two groups differ in how trials are assigned across counterbalancing blocks.

Experiment AS is a supplementary asymptote-search study (referred to as "AS" or "Experiment 9a" in the analysis code); it is reported in the supplementary materials rather than the main paper.

---

## File locations

### `scenarios/`

| File | Experiment | Condition / notes |
|------|-----------|-------------------|
| `exp1a_scenarios.xml` | Experiment 1a | Single scenario set |
| `exp1b_scenarios.xml` | Experiments 1b and 2 (moral) | Shared scenario set; see note below |
| `exp2_scenarios_moral.xml` | Experiment 2 | Moral-choice version |
| `exp2_scenarios_economic.xml` | Experiment 2 | Economic-choice version |
| `exp3_scenarios_moral.xml` | Experiment 3 | Moral-choice version |
| `exp3_scenarios_economic.xml` | Experiment 3 | Economic-choice version |
| `exp3_scenarios_evil.xml` | Experiment 3 | Evil-choice version (nepotism) |
| `exp3_scenarios_evil_ingroup.xml` | Experiment 3 | Evil-choice version with in-group favoritism |
| `exp4_scenarios.xml` | Experiment 4 | Incommensurate-scenario version |
| `expAS_scenarios.xml` | Experiment AS (asymptote search) | Incommensurate-scenario version, 10 scenarios |
| `exp5_scenarios.xml` | Experiment 5 | Incommensurate-scenario version, new ratios |
| `exp6_scenarios.xml` | Experiment 6 | Replication of Experiment 5 |

**Note:** The moral condition of Experiment 2 used the same scenario file as Experiment 1b (`exp1b_scenarios.xml`). The file is therefore listed under both experiments and appears once in the folder.

### `number_conditions/`

| File | Experiment | Condition / notes |
|------|-----------|-------------------|
| `exp1a_number_conditions.csv` | Experiment 1a | |
| `exp1b_number_conditions.csv` | Experiment 1b | |
| `exp2_number_conditions.csv` | Experiment 2 | |
| `exp3a_number_conditions.csv` | Experiment 3a | Ratio vs. harm |
| `exp3b_number_conditions.csv` | Experiment 3b | Ratio vs. utility |
| `exp3c_number_conditions.csv` | Experiment 3c | Ratio vs. utility, less extreme contrast |
| `exp4_number_conditions_group1.csv` | Experiment 4 | Counterbalancing group 1 |
| `exp4_number_conditions_group2.csv` | Experiment 4 | Counterbalancing group 2 |
| `expAS_number_conditions_group1.csv` | Experiment AS (asymptote search) | Counterbalancing group 1 |
| `expAS_number_conditions_group2.csv` | Experiment AS (asymptote search) | Counterbalancing group 2 |
| `exp5_number_conditions_group1.csv` | Experiment 5 | Counterbalancing group 1 |
| `exp5_number_conditions_group2.csv` | Experiment 5 | Counterbalancing group 2 |
| `exp6_number_conditions_group1.csv` | Experiment 6 | Counterbalancing group 1 |
| `exp6_number_conditions_group2.csv` | Experiment 6 | Counterbalancing group 2 |

---

## Original file locations

All files are copies of originals located under `experiments/` in the repository root. The table below gives the original path (relative to the repository root) for each file.

### Scenarios

| File in `materials/scenarios/` | Original path |
|--------------------------------|---------------|
| `exp1a_scenarios.xml` | `experiments/experiment1/scenarios.xml` |
| `exp1b_scenarios.xml` | `experiments/experiment1b/scenarios_replication.xml` |
| `exp2_scenarios_moral.xml` | `experiments/experiment1b/scenarios_replication.xml` |
| `exp2_scenarios_economic.xml` | `experiments/experiment4/scenarios_replication_economic.xml` |
| `exp3_scenarios_moral.xml` | `experiments/experiment2_3/final/scenarios_xml/Scenarios_Moral_Choice_Version.xml` |
| `exp3_scenarios_economic.xml` | `experiments/experiment2_3/final/scenarios_xml/Scenarios_Economic_Choice_Version.xml` |
| `exp3_scenarios_evil.xml` | `experiments/experiment2_3/final/scenarios_xml/Scenarios_Evil_Choice_Version.xml` |
| `exp3_scenarios_evil_ingroup.xml` | `experiments/experiment2_3/final/scenarios_xml/Scenarios_Evil_Choice_Version_Ingroup_favoritism.xml` |
| `exp4_scenarios.xml` | `experiments/experiment8_incommensurate_scenarios/incommensurate_scenarios_single_horrificness_question.xml` |
| `expAS_scenarios.xml` | `experiments/experiment9_verify_asymptote/incommensurate_scenarios_single_horrificness_question.10.scenarios.xml` |
| `exp5_scenarios.xml` | `experiments/experiment10_incommensurate_scenarios_new_ratios/incommensurate_scenarios_single_horrificness_question.new_questions.scenarios.exp10.xml` |
| `exp6_scenarios.xml` | `experiments/experiment11_incommensurate_scenarios_new_ratios_replication_of_exp10/incommensurate_scenarios_single_horrificness_question.new_questions.scenarios.exp10.replication.xml` |

### Number conditions

| File in `materials/number_conditions/` | Original path |
|----------------------------------------|---------------|
| `exp1a_number_conditions.csv` | `experiments/experiment1/number_conds.csv` |
| `exp1b_number_conditions.csv` | `experiments/experiment1b/number_conds_replication.csv` |
| `exp2_number_conditions.csv` | `experiments/experiment4/number_conds_replication.csv` |
| `exp3a_number_conditions.csv` | `experiments/experiment2_3/final/number_conditions/number_conds_2x2_choices_ratio_vs_victims.csv` |
| `exp3b_number_conditions.csv` | `experiments/experiment2_3/final/number_conditions/number_conds_2x2_choices_ratio_vs_utility.csv` |
| `exp3c_number_conditions.csv` | `experiments/experiment2_3/final/number_conditions/number_conds_2x2_choices_ratio_vs_utility_1.5utility_3xvictims.csv` |
| `exp4_number_conditions_group1.csv` | `experiments/experiment8_incommensurate_scenarios/number_conds_exp8_counterbalancing_group1.csv` |
| `exp4_number_conditions_group2.csv` | `experiments/experiment8_incommensurate_scenarios/number_conds_exp8_counterbalancing_group2.csv` |
| `expAS_number_conditions_group1.csv` | `experiments/experiment9_verify_asymptote/number_conds_exp9a_counterbalancing_group1.csv` |
| `expAS_number_conditions_group2.csv` | `experiments/experiment9_verify_asymptote/number_conds_exp9a_counterbalancing_group2.csv` |
| `exp5_number_conditions_group1.csv` | `experiments/experiment10_incommensurate_scenarios_new_ratios/number_conds_exp10_counterbalancing_group1.csv` |
| `exp5_number_conditions_group2.csv` | `experiments/experiment10_incommensurate_scenarios_new_ratios/number_conds_exp10_counterbalancing_group2.csv` |
| `exp6_number_conditions_group1.csv` | `experiments/experiment11_incommensurate_scenarios_new_ratios_replication_of_exp10/number_conds_exp10_counterbalancing_group1.csv` |
| `exp6_number_conditions_group2.csv` | `experiments/experiment11_incommensurate_scenarios_new_ratios_replication_of_exp10/number_conds_exp10_counterbalancing_group2.csv` |
