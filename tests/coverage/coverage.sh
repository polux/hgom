#!/bin/sh

HGOM=../../dist/build/hgom/hgom

$HGOM t1.gom 
mv hgom.tix hgom1.tix 
$HGOM t2.gom 
mv hgom.tix hgom2.tix 
$HGOM t3.gom 
mv hgom.tix hgom3.tix 
$HGOM --help
mv hgom.tix hgom4.tix 
$HGOM -r t1.gom 
mv hgom.tix hgom5.tix 
$HGOM -h -v t1.gom 
mv hgom.tix hgom6.tix 
$HGOM -p aa.bb.cc t1.gom 
mv hgom.tix hgom7.tix
$HGOM -V
mv hgom.tix hgom8.tix
$HGOM aa -p 
mv hgom.tix hgom9.tix
hpc sum hgom*.tix > hgom_sum.tix
hpc markup --srcdir=../.. --destdir=html hgom_sum.tix 

rm -rf *.tix foo aa
