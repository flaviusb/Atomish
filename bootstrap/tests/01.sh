#!/bin/bash

../bin/preatomish "`pwd`/simple-regression.atomish" > simple-regression.1
diff simple-regression.1 simple-regression.2
