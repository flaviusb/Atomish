#!/bin/bash

cd ..
./preatomish.sh "`pwd`/tests/feature-regression.atomish" features "Hello World!" > tests/feature-regression.1
cd -
diff feature-regression.1 feature-regression.2
