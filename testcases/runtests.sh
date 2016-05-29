#!/usr/bin/env bash

bin="$1"

for test in testcases/*.tig; do
	echo $test
	diff <($bin <$test 2>&1) ${test%.tig}.out
done
