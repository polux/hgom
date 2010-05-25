#!/bin/sh

hgom -p jboss_el --oomapping jboss_el/EL.gom
tom --genIntrospector jboss_el/TestEL.t
javac -cp ../../test/data/junit.jar:../../test/data/tom_runtime.jar:lib/jboss-el.jar:lib/el-api.jar:lib/tomcat-piece.jar: jboss_el/TestEL.java
