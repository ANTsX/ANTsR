#!/bin/bash
for x in ../man/*Rd ; do 
  y=${x%.Rd}
  y=` basename $y `
  onm=./source/Rd_${y}.rst 
  if [[ ! -s ${onm} ]] ; then 
    ./scripts/convertRd2rst.R $x $onm
  fi 
done
