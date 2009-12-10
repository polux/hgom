#!/bin/bash

rm -rf big
rm -f big.gom
rm -f hgom_time.data hgom_space.data
rm -f gom_time.data gom_space.data

for i in `seq 1 100`; do
  ./Gen $i > big.gom
  /usr/bin/time -f '%e' -o hgom_time.data -a hgom big.gom
  sloccount big | grep 'java:' | awk '{print $2}' >> hgom_space.data
  rm -rf big
  /usr/bin/time -f '%e' -o gom_time.data -a gom big.gom
  sloccount big | grep 'java:' | awk '{print $2}' >> gom_space.data
  rm -rf big
  rm big.gom
done
