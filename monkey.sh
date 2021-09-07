#!/bin/bash

mkdir -p fuzz_tests

if [ ! -f fuzz_tests/fuzz_count.txt ]
then
    echo 0 > fuzz_tests/fuzz_count.txt
fi

count=`cat fuzz_tests/fuzz_count.txt`

echo $count

while true
do
    #echo Fuzzing \#$count

    fuzz_src="fuzz_tests/fuzz${count}.sy"
    python3 misc/fuzz.py  | clang-format | sed 's/\-\-/\-/g' > $fuzz_src
    misc/test.py -t $fuzz_src -n
    misc/test.py -t $fuzz_src

    ((count++))
    echo $count > fuzz_tests/fuzz_count.txt
done
