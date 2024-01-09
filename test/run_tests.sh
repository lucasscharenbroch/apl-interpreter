#!/bin/bash

RED=$'\033[0;31m'
GREEN=$'\033[0;32m'
NO_COLOR=$'\033[0;0m'

arg=${1:--w}

usage() {
    echo "USAGE: $0 [OPTIONS]"
    echo "-v  vimdiff"
    echo "-Z  ignore spaces at end of lines"
    echo "-w  ignore all whitespace"
}

if [ $arg = "-v" ]; then
    DIFF_CMD="vimdiff"
elif [ $arg = "-Z" ]; then
    DIFF_CMD="diff -Z"
elif [ $arg = "-w" ]; then
    DIFF_CMD="diff -w"
else
    echo "Invalid argument: $arg"
    usage
    exit 1
fi

COL1=71

dyalog_test_files=(
        "general_fns"
        "scan_reduce"
        "rand"
        "index"
        "exec_format"
        "general_ops"
        "combinators"
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

manual_test_files=(
        "misc_err"
        "index_err"
    )

total_passed=0

time {
    echo "# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ Dyalog As Oracle ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #"

    for file in ${dyalog_test_files[*]}; do
        S="Running ${file}... "
        echo -n "$S"

        box_prefix=$']box on -trains=tree\n'

        ai_out=$(../bin/ai <$file)
        dyalog_out=$( (dyalog <<<"${box_prefix}$(cat $file)" 2>/dev/null | tail +2 ) )

        $DIFF_CMD <(echo "$ai_out") <(echo "$dyalog_out")

        if [ $? -eq 0 ]; then
            ((++total_passed))
            RES=${GREEN}PASS${NO_COLOR}
            printf "%*s\n" $((COL1 - ${#S})) $RES
        else
            RES=${RED}FAIL${NO_COLOR}
            printf "\n%*s\n" $((COL1)) $RES
        fi
    done

    echo "# ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ Manual Oracle ~ ~ ~ ~ ~ ~ ~ ~ ~ ~ #"

    for file in ${manual_test_files[*]}; do
        S="Running ${file}... "
        echo -n "$S"

        ai_out=$(../bin/ai <$file 2>&1)
        oracle_file="${file}_oracle"

        $DIFF_CMD <(echo "$ai_out") $oracle_file

        if [ $? -eq 0 ]; then
            ((++total_passed))
            RES=${GREEN}PASS${NO_COLOR}
            printf "%*s\n" $((COL1 - ${#S})) $RES
        else
            RES=${RED}FAIL${NO_COLOR}
            printf "\n%*s\n" $((COL1)) $RES
        fi
    done
}

echo "Total passed: ( $total_passed / $(( ${#dyalog_test_files[*]} + ${#manual_test_files[*]})) )"
