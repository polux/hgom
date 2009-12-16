#!/bin/sh

HGOM=../../dist/build/hgom/hgom

for i in t*.gom; do
  $HGOM $i
done 

$HGOM --help
$HGOM -r t1.gom 
$HGOM -c no t1.gom 
$HGOM -c sep t1.gom 
$HGOM -c same t1.gom 
$HGOM -c foo t1.gom 
$HGOM -h t1.gom 
$HGOM --noVisitable t1.gom
$HGOM --noSharing t1.gom
$HGOM --noCheck t1.gom
$HGOM -p aa.bb.cc t1.gom 
$HGOM -V
$HGOM aa -p 
$HGOM
$HGOM aa bb

hpc markup --srcdir=../.. --destdir=html hgom.tix 

rm -rf hgom.tix foo aa
