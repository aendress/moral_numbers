#!/bin/sh

# We use only 10 scenarios, as the bomb on train and the hospital power outage don't work for really large numbers

# Vivid, number counterbalancing group 1
# Arguments
# scenario file, number condition, scenario filter string, block prefix

# Not needed for experiment 9a
#./parse_scenarios_2questions_with_filter.with_formatted_numbers.pl incommensurate_scenarios_single_horrificness_question.xml number_conds_exp8_counterbalancing_group1.csv vivid scenarios.vivid.cb1
#mv incommensurate_scenarios_single_horrificness_question.qualtrics.txt incommensurate_scenarios_single_horrificness_question.qualtrics.vivid_cb1.txt

# Vivid, number counterbalancing group 2
#./parse_scenarios_2questions_with_filter.with_formatted_numbers.pl incommensurate_scenarios_single_horrificness_question.xml number_conds_exp8_counterbalancing_group2.csv vivid scenarios.vivid.cb2
#mv incommensurate_scenarios_single_horrificness_question.qualtrics.txt incommensurate_scenarios_single_horrificness_question.qualtrics.vivid_cb2.txt


# Neutral, number counterbalancing group 1
./parse_scenarios_2questions_with_filter.with_formatted_numbers.pl incommensurate_scenarios_single_horrificness_question.10.scenarios.xml number_conds_exp9a_counterbalancing_group1.csv neutral scenarios.neutral.cb1
mv incommensurate_scenarios_single_horrificness_question.10.scenarios.qualtrics.txt incommensurate_scenarios_single_horrificness_question.10.scenarios.qualtrics.exp9a.neutral_cb1.txt

# Neutral, number counterbalancing group 2
./parse_scenarios_2questions_with_filter.with_formatted_numbers.pl incommensurate_scenarios_single_horrificness_question.10.scenarios.xml number_conds_exp9a_counterbalancing_group2.csv neutral scenarios.neutral.cb2
mv incommensurate_scenarios_single_horrificness_question.10.scenarios.qualtrics.txt incommensurate_scenarios_single_horrificness_question.10.scenarios.qualtrics.exp9a.neutral_cb2.txt
