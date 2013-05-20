Tests for the Atomish Bootstrapper
==================================

This directory contains tests for the Atomish Bootstrapper. The entry point for the tests is `tests.sh`, which shells out to other shell scripts to perform each of the sets of tests. There is no test harness yet, and test set failures are indicated by a non-zero return code from the test set script. Total test set failures are printed to stdout at the end of a run.

At the moment the tests are fairly primitive - they are pass/fail for a whole set, based on textual diffing. I would like to improve this.
