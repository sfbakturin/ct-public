#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

N=$1

for ((i=1; i<=$N; i++))
do
	./data "file${i}"
done


flag=$(ps | grep -o "data")
while [ -n "$flag" ];
do
	sleep 0.1s
	flag=$(ps | grep -o "data")
done
