#!/bin/tcsh

hgom -p jboss_el --oomapping jboss_el/EL.gom
tom jboss_el/TestEL.t
javac -cp lib/tom-runtime-full.jar:lib/jboss-el.jar:lib/el-api.jar:lib/tomcat-piece.jar jboss_el/TestEL.java
