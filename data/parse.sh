#!/bin/sh
#
# 


awk -F '\t' '{print "\x27"$2"\x27", $9, $7, $8}' $1 | grep -v "''" > $2-2011.dat
awk -F '\t' '{print "\x27"$13"\x27", $10, $11, $12}' $1 | grep -v "''"  > $2-2007.dat

