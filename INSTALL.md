Installing Atomish
------------------

At the moment, you have to build Atomish from source. Also, only the bootstrapping pre-interpreter exists, which interprets a pre-Atomish language.

You will need a Java 7 sdk, and a recent Scala (I use 2.10.1 for development). Both should be available from your package manager, or as direct downloads from [Oracle](oracle.com) (probably [Technetwork downloads](http://www.oracle.com/technetwork/indexes/downloads/index.html)) and [http://www.scala-lang.org/](http://www.scala-lang.org/).

Then from the project root:

    cd bootstrap
    ./mk.sh

Once you have built it, the preinterpreter can be invoked through a shellscript found at `bootstrap/bin/preatomish`.

The script `bootstrap/lazyinstall.sh`, if invoked from within `bootstrap` (and with the necessary permissions, so probably as `cd bootstrap; sudo ./lazyinstall.sh`), will make a symlink from `/usr/bin/preatomish` to `bootstrap/bin/preatomish`, thus putting preatomish on `$PATH` in the laziest way possible.
