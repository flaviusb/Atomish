Installing Atomish
------------------

At the moment, you have to build Atomish from source. Also, only the bootstrapping pre-interpreter exists, which interprets a pre-Atomish language.

You will need a Java 7 sdk, and a recent Scala (I use 2.10.1 for development). Both should be available from your package manager, or as direct downloads from [Oracle](oracle.com) (probably [Technetwork downloads](http://www.oracle.com/technetwork/indexes/downloads/index.html)) and [http://www.scala-lang.org/](http://www.scala-lang.org/).

Then from the project root:

    cd bootstrap
    ./mk.sh

The preinterpreter can be invoked through a shellscript found at `bootstrap/preatomish.sh`

