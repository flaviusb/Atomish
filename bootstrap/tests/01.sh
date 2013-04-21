#!/bin/bash

cd ..
./preatomish.sh "`pwd`/tests/simple-regression.atomish" > tests/simple-regression.1
cd -
diff simple-regression.1 simple-regression.2
