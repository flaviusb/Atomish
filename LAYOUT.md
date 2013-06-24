Project Layout
-------------

The bootstrap interpreter is written in Scala, and interprets a pre-atomish language which is powerful enough to write the actual atomish interpreter and compiler in, but significantly simpler than the full atomish language. It is located in the `bootstrap` subdirectory.

The Atomish compiler and interpreter are written in a multi-level style; each level has a corresponding directory - that is, the code is split across the `preatomish`, `atomish`, `sugar`, and `library` directories. There are no dependancies from lower levels on higher levels.

Tests are always in the `tests` subdirectory of the directory being tested. Tests can always be started by running the script `tests.sh` in the test directory.
