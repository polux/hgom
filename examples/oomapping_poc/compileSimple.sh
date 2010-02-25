#!/bin/sh

hgom -p simple --oomapping simple/Signature.gom
tom simple/Test.t
javac -cp ../../test/data/tom-runtime-full.jar: simple/Test.java
