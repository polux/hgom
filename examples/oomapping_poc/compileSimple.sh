#!/bin/tcsh

hgom -p simple --oomapping simple/Signature.gom
tom simple/Test.t
javac -cp lib/tom-runtime-full.jar simple/Test.java
