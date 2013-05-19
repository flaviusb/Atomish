#!/bin/bash
let errorCounter=0
./01.sh
if [ $? -ne 0 ]
then
  errorCounter=$(($errorCounter+1))
fi
./02.sh
if [ $? -ne 0 ]
then
  errorCounter=$(($errorCounter+1))
fi

tests="test"
if [ $errorCounter -ne 1 ]
then
  tests="tests"
fi
echo "$errorCounter $tests failed."
exit $errorCounter
