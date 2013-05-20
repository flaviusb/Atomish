Vendored libraries for the bootstrap interpreter
------------------------------------------------

This directory contains 'Vendored' libraries used by the bootstrap interpreter. 'Vendoring' is a process whereby some dependencies of a project are
copied into the source control of that project, usually all into a specific directory (in this case, the directory `bootstrap/vendor`).
Vendoring is an explicit trade off - the build process becomes more involved, and you lose some of the benefits of package management
and build management systems, but in exchange the project becomes somewhat more self-contained.

The only vendored dependency for the bootstrap compiler is jRegex.
