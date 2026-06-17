# List of experiments

Each experiment entry uses the following fields:

- **Experiment ID in paper**: the experiment number as it appears in the manuscript
- **Data ID**: the experiment identifier used in the R code and data files (e.g. in the `experimentID` column and in `dat.moral.numbers.exp.correspondance`)
- **Experiment files folder**: the folder under `experiments/` containing the scenario XML files, number-condition CSVs, and Perl scripts used to generate the Qualtrics survey
- **Qualtrics (QSF) file**: the Qualtrics survey file as referenced in `moral_numbers.Rmd`; all QSF files are in `data/qualtrics_qsf_files/`

## Experiments included in the paper

### Experiment 1
Experiment ID in paper: 1a
Data ID: exp1
Experiment files folder: `experiments/experiment1`
Qualtrics (QSF) file: not referenced in Rmd (Aiken format only: `experiments/experiment1/scenarios.qualtrics.txt`)

Initial pilot. Tested whether acceptability tracks the beneficiary:victim ratio (Weber's law), whether small numbers allow exact discrimination, and whether the first victim has special status.

The following number conditions were used: 

n_victims	n_saved	n_total
2	4	6
3	4	7
2	3	5
20	40	60
30	40	70
20	30	50
200	400	600
300	400	700
200	300	500
1	40	41
2	40	42
10	40	50

### Experiment 1b
Experiment ID in paper: 1b
Data ID: exp1b
Experiment files folder: `experiments/experiment1b`
Qualtrics (QSF) file: not referenced in Rmd (Aiken format only: `experiments/experiment1b/scenarios_replication.qualtrics.txt`)

Replication of Experiment 1 with a new set of ratios.

n_victims	n_saved	n_total
300	400	700
20	30	50
200	400	600
10	30	40
100	400	500
30	300	330
400	6000	6400
30	600	630
400	12000	12400
20	800	820

### Experiment 4
Experiment ID in paper: 2
Data ID: exp4
Experiment files folder: `experiments/experiment4`
Qualtrics (QSF) file: `Moral_Numbers_4_-_Exp_1b_with_moral_vs_economic_choices__Exp_3d_EvilIngroup_favoritism.qsf`

Replication of Experiment 1b adding an economic version of the task. The moral version reuses the Experiment 1b scenarios. Note: the QSF also includes the Experiment 3d (evil ingroup) conditions.

### Experiment 2
Experiment ID in paper: 3 (data 2b → paper 3a; data 2a → paper 3b.1; data 2c → paper 3b.2; data 2a+2c combined → paper 3b; data 2d → paper 3c)
Data ID: exp2a, exp2b, exp2c, exp2d
Experiment files folder: `experiments/experiment2_3`
Qualtrics (QSF) file (one per sub-experiment, each also includes Experiment 3 evil-choice conditions):
- `Moral_Numbers_2a-MoralEconomic_choices-ratios_vs_utility__evil_choice.qsf`
- `Moral_Numbers_2b-MoralEconomic_choices-ratios_vs_number_of_victims__evil_choice.qsf`
- `Moral_Numbers_2c-MoralEconomic_choices-ratios_vs_utility__evil_choice_-_Replication.qsf`
- `Moral_Numbers_2d3b-MoralEconomic_choices-ratios_vs_utility_replication_with_less_extreme_contrast.qsf`

Forced-choice paradigm pitting ratio against number of victims and utility, in both moral and economic versions.

### Experiment 8
Experiment ID in paper: 4
Data ID: exp8
Experiment files folder: `experiments/experiment8_incommensurate_scenarios`
Qualtrics (QSF) file: `Moral_Numbers_Exp_8_-_vivid_vs_neutral_with_incomensurate_scenarios_-_attention_check_corrected.qsf`

First successful vivacity manipulation. Vivid vs. neutral incommensurate scenarios, 12 trials per block, two counterbalanced blocks.

### Experiment 9
Experiment ID in paper: AS (reported in supplementary materials)
Data ID: exp9a
Experiment files folder: `experiments/experiment9_verify_asymptote`
Qualtrics (QSF) file: `Moral_Numbers_Exp_9a_-_verifying_asymptote_in_neutral_condition.qsf`

Supplementary asymptote search. Vivid condition only, with extreme ratios up to 1:20,000 and 10 scenarios (down from 12).

### Experiment 10
Experiment ID in paper: 5
Data ID: exp10
Experiment files folder: `experiments/experiment10_incommensurate_scenarios_new_ratios`
Qualtrics (QSF) file: `Moral_Numbers_Exp_10_-_vivid_vs_neutral_with_incomensurate_scenarios_higher_ratios.qsf`

Replication of Experiment 8 with higher ratios (up to 1:200), since the asymptote was not reached in Experiment 8.

### Experiment 11
Experiment ID in paper: 6
Data ID: exp11
Experiment files folder: `experiments/experiment11_incommensurate_scenarios_new_ratios_replication_of_exp10`
Qualtrics (QSF) file: `exp11_replication_of_exp10_vivid_vs_neutral.qsf`

Direct replication of Experiment 10 with clarified scenarios.

---

## Experiments not included in the paper

### Experiment 3
Experiment ID in paper: not included
Data ID: not separately defined; nepotism conditions bundled within experiments 2a/2b/2c
Experiment files folder: `experiments/experiment2_3`
Qualtrics (QSF) file: bundled in the Experiment 2 QSF files (see above)

Tested how acceptable it is to sacrifice a greater number due to nepotism. Run after Experiment 2.

#### Experiment 3b

Lower ratio scenarios had a less negative utility and higher number of victims, so participants might have preferred them to minimize the ratio or maximize the utility. Experiment 3b pits utility and ratio directly against one another by giving the lower ratio scenario more negative utility (and a greater number of victims).

#### Experiment 3c

Identical to Experiment 3a except that the immoral decisions are based on in-group favoritism rather than personal friendship.

#### Experiment 3d

Identical to Experiment 3b except that the immoral decisions are based on in-group favoritism.

### Experiment 5
Experiment ID in paper: not included
Data ID: exp5, exp5b
Experiment files folder: `experiments/experiment5`
Qualtrics (QSF) file:
- `Moral_Numbers_5_-_vivid_and_neutral_moral_choices__Exp_3d_EvilIngroup_favoritism.qsf` (Experiment 5a; also includes Experiment 3d conditions)
- `Moral_Numbers_5b_-_vivid_and_neutral_moral_choices_-_1_block_only_-_different_ratios.qsf` (Experiment 5b)

Failed vivacity pilot. Added victim-salience sentences, but victims and beneficiaries died the same way so the vivid/neutral distinction had no effect. Experiment 5b used one block per participant with more asymptotic ratios and bold vivid text.

### Experiment 6
Experiment ID in paper: not included
Data ID: exp6
Experiment files folder: `experiments/experiment6`
Qualtrics (QSF) file: `Moral_Numbers_6_-_vivid_and_neutral_moral_choices.qsf`

Failed vivacity pilot. Third-person neutral vs. first-person vivid. Excluded because there was no severity check.

### Experiment 7
Experiment ID in paper: not included
Data ID: exp7
Experiment files folder: `experiments/experiment7`
Qualtrics (QSF) file: `Moral_Numbers_Exp_7_-_vivid_vs_neutral_with_incomensurate_scenarios.qsf`

Failed vivacity pilot. Like Experiment 6 but with a severity check. Excluded because too many participants did not rate victims' deaths as more severe than beneficiaries' deaths.

---

# File locations

## Helper functions

Reusable R functions are organised by theme in `code/helper_functions/`. All files in that directory are sourced automatically when `moral_numbers.Rmd` is rendered. See `code/helper_functions/README.md` for a full description of each file and the functions it contains.

## Child RMDs

`code/child_rmds/` contains child RMD files that are knitted into the main document (`code/moral_numbers.Rmd`).

## Experiment materials

Scenario files (XML) and number-condition files (CSV) for all experiments are collected in the `materials/` folder:

- `materials/scenarios/` — XML files with scenario text and questions for each experiment
- `materials/number_conditions/` — CSV files specifying the numerical values (number saved, number harmed, group size) for each trial

Files are named using the paper's experiment numbering (e.g., `exp1a_scenarios.xml`, `exp3b_number_conditions.csv`). See `materials/README.md` for a full description of file contents, naming conventions, and the original source locations within the repository.


# Stuff to note somewhere else

Given that we have long question names on qualtrics, we need to export the qsf files to extract the names
