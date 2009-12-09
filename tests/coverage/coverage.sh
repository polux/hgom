#!/bin/sh

HGOM=../../dist/build/hgom/hgom

$HGOM t1.gom 
$HGOM t2.gom 
$HGOM t3.gom 
$HGOM t4.gom 
$HGOM t5.gom 
$HGOM t6.gom 
$HGOM t7.gom 
$HGOM t8.gom 
$HGOM t9.gom 

$HGOM --help
$HGOM -r t1.gom 
$HGOM -h -v t1.gom 
$HGOM -p aa.bb.cc t1.gom 
$HGOM -V
$HGOM aa -p 
$HGOM
$HGOM aa bb

hpc markup --srcdir=../.. --destdir=html hgom.tix 

rm -rf hgom.tix foo aa
