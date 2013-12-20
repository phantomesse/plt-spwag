#!/bin/bash

if [ $# -eq 0 ]
    then
    echo ""
    echo "TEST CASES"
    echo "----------"

    echo "Making the SPWAG language..."
    make

    testcases[0]='hello-world'
    testcases[1]='one-fish-two-fish'
    testcases[2]='presentation'
    testcases[3]='fiver'

    for testcase in "${testcases[@]}"
    do
        input='testcases/'$testcase'/test.spwag'
        output='testcases/'$testcase'/output/index.html'

        echo ""
        echo "Testing "$testcase"..."
        ./spwag -i < $input > $output
    done

    echo ""
    exit
fi

make
input='testcases/'$1'/test.spwag'
output='testcases/'$1'/output/index.html'

echo ""
echo "Testing "$1"..."
./spwag -i < $input > $output
