#!/bin/bash

rm -rf big
rm -f big.gom
rm -f time_ratio.data space_ratio.data

for i in `seq 1 1000`; do
  ./Gen $i > big.gom
  t1=`/usr/bin/time -f '%e' hgom big.gom 2>&1`
  s1=`sloccount big | grep 'java:' | awk '{print $2}'`
  rm -rf big
  t2=`/usr/bin/time -f '%e' gom big.gom 2>&1`
  s2=`sloccount big | grep 'java:' | awk '{print $2}'`
  rm -rf big
  rm big.gom
  t=`echo "$t2/$t1" | bc -l`
  s=`echo "$s2/$s1" | bc -l`
  echo $t >> time_ratio.data
  echo $s >> space_ratio.data
done
