#!/bin/sh

hgom t1.gom 
mv hgom.tix hgom1.tix 
hgom t2.gom 
mv hgom.tix hgom2.tix 
hgom t3.gom 
mv hgom.tix hgom3.tix 
hgom --help
mv hgom.tix hgom4.tix 
hgom -r t1.gom 
mv hgom.tix hgom5.tix 
hgom -h -v t1.gom 
mv hgom.tix hgom6.tix 
hgom -p aa.bb.cc t1.gom 
mv hgom.tix hgom7.tix
hgom -V
mv hgom.tix hgom8.tix
hgom aa -p 
mv hgom.tix hgom9.tix
hpc sum hgom*.tix > hgom_sum.tix
hpc markup --srcdir=../.. --destdir=html hgom_sum.tix 

rm -rf *.tix foo aa
