#!/bin/tcsh

hgom -p oomapping --oomapping oomapping/Signature.gom
tom oomapping/OOMappingTest.t
javac -cp lib::lib/tom-runtime-full.jar oomapping/OOMappingTest.java
