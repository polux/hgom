#!/bin/sh

hgom -p simple --oomapping simple/Signature.gom
tom simple/Test.t
javac -cp ../../test/data/tom_runtime.jar:../../test/data/junit.jar: simple/Test.java
