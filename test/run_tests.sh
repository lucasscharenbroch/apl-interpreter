#!/bin/bash

test_files=(
        "deeply_nested_parens"
        "quad_gets"
        "modified_assignment"
        "numeric_fns"
        "simple_fns"
        "dfns_dops"
        "multi_expr_line"
        "function_tree"
        "basic_assignment"
        "float_literals"
        "shape"
        "iota"
        "comments"
        "add_array_literals"
        "array_literal"
    )

for file in ${test_files[*]}; do
    echo "Running ${file}..."

    box_prefix=$']box on -trains=tree\n'

    ai_out=$(../bin/ai <$file)
    dyalog_out=$( (dyalog <<<"${box_prefix}$(cat $file)" 2>/dev/null | tail +2 ) )

    arg=${1:--w}

    if [ $arg = "-v" ]; then
        vimdiff <(echo "$ai_out") <(echo "$dyalog_out")
    elif [ $arg = "-Z" ]; then
        diff -Z <(echo "$ai_out") <(echo "$dyalog_out")
    elif [ $arg = "-w" ]; then
        diff -w <(echo "$ai_out") <(echo "$dyalog_out")
    else
        echo "invalid argument: $arg"
    fi;
done
