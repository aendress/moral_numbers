#!/bin/bash


# Input and base filenames
SCENARIO_FILE="incommensurate_scenarios_single_horrificness_question.new_questions.scenarios.exp10.replication.xml"
OUTPUT_BASE="incommensurate_scenarios_single_horrificness_question.new_questions.scenarios.exp10.replication.qualtrics"


# Counterbalancing groups and vividness conditions
#unset GROUPS
NUMBER_GROUPS=("group1" "group2")
CONDITIONS=("vivid" "neutral")

for cond in "${CONDITIONS[@]}"; do
    for group in "${NUMBER_GROUPS[@]}"; do
        echo "Processing: Condition=$cond, Group=$group"
        
        # Run parser
        ./parse_scenarios_2questions_with_filter.with_formatted_numbers.pl \
            "$SCENARIO_FILE" \
            "number_conds_exp10_counterbalancing_${group}.csv" \
            "$cond" \
            "scenarios.${cond}.cb${group: -1}"
        
        # Rename output
        mv "${OUTPUT_BASE}.txt" "${OUTPUT_BASE}.${cond}_cb${group: -1}.txt"
    done
done
