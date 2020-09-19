#!/bin/bash

TESTS="-c -i -d -r -x"
BACKENDS="powerpdf fclpdf htmldiv txt csv jpg bmp png xls xlsx ods"

runtest() {
	echo "reporter.exe $1 --back=$2"
	./reporter $1 --back=$2 > $2$1.log 2>&1
}

for b in $BACKENDS
do
	for t in $TESTS
	do
		runtest $t $b
	done
done
