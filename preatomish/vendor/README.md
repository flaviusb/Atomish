Vendored libraries for the preatomish-level compiler
----------------------------------------------------

This directory contains 'Vendored' libraries used by the preatomish level of the compiler. 'Vendoring' is a process whereby some dependencies of a project are
copied into the source control of that project, usually all into a specific directory (in this case, the directory `preatomish/vendor`).
Vendoring is an explicit trade off - the build process becomes more involved, and you lose some of the benefits of package management
and build management systems, but in exchange the project becomes somewhat more self-contained. In this particular case, we need access to prerelease features of llvm, so we are addin that as a git submodule.

The only vendored dependency for the preatomish-level compiler is llvm.
