#!/bin/bash

test_files=(
        "array_literal"
    )

for file in ${test_files}; do
    echo "Running ${file}..."

    box_prefix=$']box on\n'

    ai_out=$(../bin/ai <$file)
    dyalog_out=$( (dyalog <<<"${box_prefix}$(cat $file)" 2>/dev/null | tail +2 ) )

    sdiff <(echo "$ai_out") <(echo "$dyalog_out")
done
