#!/bin/bash

PROGRAM="./spl"

test_output=0

single_test()
{
    local _test=$1
    local _exec=${_test%*.spl}
    dir=$(dirname $_test)
    echo "#### testing $_test"

    ${PROGRAM} $_test 2>&1
    local _retcode=$?
    if [ ${_retcode} != 0 ]; then
        echo -e "\e[31mSPL failed\e[39m"
        test_output=0
    else
        if [ ! -f ${_exec}.in ]; then
            out=$(${_exec})
        else
            out=$(${_exec} < ${_exec}.in)
        fi
        true_out=$(cat ${_exec}.out)
        if [ "$out" == "$true_out" ]; then
            echo -e "\e[32mSPL passed\e[39m"
            test_output=1
        else
            echo -e "\e[31mSPL failed\e[39m"
            echo "Output:"
            echo $out
            echo ""
            echo "Correct output:"
            echo $true_out
            test_output=0
        fi
        rm ${_exec}.s ${_exec}
    fi
}

single_bad_test()
{
    local _test=$1
    _exec=${_test%*.spl}
    echo "#### testing $_test"

    ${PROGRAM} $_test 2>&1 >/dev/null
    _retcode=$?
    if [ ${_retcode} == 1 ]; then
        echo -e "\e[32mSPL passed\e[39m"
        test_output=1
    else
        echo -e "\e[31mSPL failed\e[39m"
        test_output=0
        rm ${_exec}.s ${_exec}
    fi
}

TEST_DIRS=(./tests/good/basic
           ./tests/good/array
           ./tests/good/struct
           ./tests/good/lambda
           )

BAD_TEST_DIRS=(./tests/bad
               )

FAILED_TESTS=()

FAILED_BAD_TESTS=()

if [ ! -z $1 ]; then
    single_test $1
    exit 0
fi

for test_dir in ${TEST_DIRS[*]}; do
    echo ""
    echo "######## TESTING DIRECTORY: ${test_dir}"
    echo ""
    for test_case in ${test_dir}/*.spl; do
        single_test ${test_case}
        if [ ${test_output} == "0" ]; then
            FAILED_TESTS+=(${test_case})
        fi
    done
done

for test_dir in ${BAD_TEST_DIRS[*]}; do
    echo ""
    echo "####### TESTING BAD DIRECTORY: ${test_dir}"
    echo ""
    for test_case in ${test_dir}/*.spl; do
        single_bad_test ${test_case}
        if [ ${test_output} == "0" ]; then
            FAILED_BAD_TESTS+=(${test_case})
        fi
    done
done

if [ ${#FAILED_TESTS[*]} -eq 0 ] && [ ${#FAILED_BAD_TESTS[*]} -eq 0 ]; then
    echo -e "\n\n#### ALL TESTS PASSED ####"
else
    if [ ${#FAILED_TESTS[*]} -ne 0 ]; then
        echo -e "\n\n#### FAILED GOOD TESTS"
    fi
    for t in ${FAILED_TESTS[*]}; do
        echo $t
    done

    if [ ${#FAILED_BAD_TESTS[*]} -ne 0 ]; then
        echo -e "\n\n#### FAILED BAD TESTS"
    fi
    for t in ${FAILED_BAD_TESTS[*]}; do
        echo $t
    done
fi
