#!/bin/bash

echo ""
echo "TEST CASES"
echo "----------"

echo "Making the SPWAG language..."
make clean
make

testcases[0]='hello-world'
testcases[1]='one-fish-two-fish'

for testcase in "${testcases[@]}"
do
    input='testcases/'$testcase'/test.spwag'
    output='testcases/'$testcase'/output/index.html'

    echo ""
    echo "Testing "$testcase"..."
    ./spwag -i < $input > $output
done


echo ""
