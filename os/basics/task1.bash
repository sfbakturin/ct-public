#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

touch numbers
touch sorted
for i in $*; do
	echo $i >> numbers
done
sort -n -r numbers > sorted
result=$(head -n 1 sorted)
echo $result
rm numbers
rm sorted
