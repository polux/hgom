#!/bin/sh

HGOM=../../dist/build/hgom/hgom
DATA=../data

for i in $DATA/t*.gom; do
  $HGOM $i
done 

$HGOM --help
$HGOM -P $DATA/t1.gom 
$HGOM -P $DATA/t14.gom 
$HGOM -c no $DATA/t1.gom 
$HGOM -c sep $DATA/t1.gom 
$HGOM -c same $DATA/t1.gom 
$HGOM -c foo $DATA/t1.gom 
$HGOM -j $DATA/t1.gom 
$HGOM -j -p aa.bb.cc $DATA/t14.gom 
$HGOM -j -p aa.bb.cc $DATA/t1.gom 
$HGOM -h $DATA/t1.gom 
$HGOM --test 5 $DATA/t1.gom 
$HGOM --compact $DATA/t1.gom 
$HGOM --random $DATA/t1.gom 
$HGOM --size $DATA/t1.gom 
$HGOM --depth $DATA/t1.gom 
$HGOM --noVisitable $DATA/t1.gom
$HGOM --noSharing $DATA/t1.gom
$HGOM --noCheck $DATA/t1.gom
$HGOM --noParsers $DATA/t1.gom
$HGOM -p aa.bb.cc $DATA/t1.gom 
$HGOM -V
$HGOM aa -p 
$HGOM
$HGOM aa bb

hpc markup --srcdir=../.. --destdir=html hgom.tix 

rm -rf hgom.tix foo aa
