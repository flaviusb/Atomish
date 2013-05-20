#!/bin/bash

cd vendor
javac org/jregex/*.java org/jregex/util/io/*.java
cd ..
scalac -feature -classpath ".:./vendor" PreReader.scala PreUniverse.scala AtomishThing.scala AtomishPlace.scala PreEvaller.scala PrePrinter.scala PreInterpreter.scala
