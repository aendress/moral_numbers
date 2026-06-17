#!/bin/sh

# Vivid, number counterbalancing group 1
# Arguments
# scenario file, number condition, scenario filter string, block prefix
./parse_scenarios_3questions_with_filter.with_formatted_numbers.pl incommensurate_scenarios.xml number_conds_exp7_counterbalancing_group1.csv vivid scenarios.vivid.cb1
mv incommensurate_scenarios.qualtrics.txt incommensurate_scenarios.qualtrics.vivid_cb1.txt

# Vivid, number counterbalancing group 2
./parse_scenarios_3questions_with_filter.with_formatted_numbers.pl incommensurate_scenarios.xml number_conds_exp7_counterbalancing_group2.csv vivid scenarios.vivid.cb2
mv incommensurate_scenarios.qualtrics.txt incommensurate_scenarios.qualtrics.vivid_cb2.txt


# Neutral, number counterbalancing group 1
./parse_scenarios_3questions_with_filter.with_formatted_numbers.pl incommensurate_scenarios.xml number_conds_exp7_counterbalancing_group1.csv neutral scenarios.neutral.cb1
mv incommensurate_scenarios.qualtrics.txt incommensurate_scenarios.qualtrics.neutral_cb1.txt

# Neutral, number counterbalancing group 2
./parse_scenarios_3questions_with_filter.with_formatted_numbers.pl incommensurate_scenarios.xml number_conds_exp7_counterbalancing_group2.csv neutral scenarios.neutral.cb2
mv incommensurate_scenarios.qualtrics.txt incommensurate_scenarios.qualtrics.neutral_cb2.txt
