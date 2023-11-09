#!/bin/bash

test_files=(
        "array_literal"
        "function_tree"
        "add_array_literals"
        "misc"
    )

for file in ${test_files[*]}; do
    echo "Running ${file}..."

    box_prefix=$']box on -trains=tree\n'

    ai_out=$(../bin/ai <$file)
    dyalog_out=$( (dyalog <<<"${box_prefix}$(cat $file)" 2>/dev/null | tail +2 ) )

    diff -Z <(echo "$ai_out") <(echo "$dyalog_out")
    # vimdiff <(echo "$ai_out") <(echo "$dyalog_out")
done
