#!/bin/bash

../bin/preatomish "`pwd`/feature-regression.atomish" features "Hello World!" > feature-regression.1
diff feature-regression.1 feature-regression.2
