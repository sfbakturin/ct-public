#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

N=$1
n=10000000
s=100

for ((i=1; i<=$N; i++))
do
	./function $n $s &
	n=$(("$n" + "4"))
	s=$(("$s" + "2"))
done

flag=$(ps | grep -o "function")
while [ -n "$flag" ];
do
	sleep 0.1s
	flag=$(ps | grep -o "function")
done
