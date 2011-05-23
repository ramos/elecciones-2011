#!/bin/sh
#
# 


awk -F '\t' '{print $2, $9, $7, $8}' $1 > $2-2011.dat
awk -F '\t' '{print $13, $10, $11, $12}' $1 > $2-2007.dat

