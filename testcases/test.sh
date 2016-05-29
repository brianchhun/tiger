#!/bin/bash
bin=../main.native

for test in *.tig; do
	echo $test
	diff <($bin <$test 2>&1) ${test%.tig}.out
done
