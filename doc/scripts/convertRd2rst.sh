#!/bin/bash
for x in ../man/*Rd ; do 
  y=${x%.Rd}
  y=` basename $y `
  onm=./source/Rd_${y}.rst 
  ./scripts/convertRd2rst.R $x $onm
done
