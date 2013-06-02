Atomish Bootstrap Interpreter
=============================

A simple ast walking interpreter for bootstrapping Atomish. This interpreter runs a 'pre-atomish' language, in which the 'self-hosting' interpreter and compiler will be written.

The Pre-Atomish language
------------------------

This language is both simpler than Atomish, and also does not have exactly the same semantics. The Ur-Principle *is* the same however, so there are separate reified readers, evallers, printers, runloops, universes, and mirrors. With some care, PreAtomish programs can be written that are also valid Atomish programs; the 'self-hosting' interpreter and compiler should be, for instance.

Running PreAtomish Programs
---------------------------

The Preatomish interpreter can be invoked with the script in `./bin/preatomish`. If it is on the `$PATH`, a program called `script.atomish` could be invoked like so:

~~~
preatomish script.atomish
~~~
