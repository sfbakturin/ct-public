#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

COUNTER=0
ARRAY=()
while true
do
	ARRAY+=(1 2 3 4 5 6 7 8 9 10)
	((COUNTER++))
	MOD=$(( $COUNTER % 100000 ))
	if [[ "$MOD" -eq "0" ]]
	then
		echo -e "${#ARRAY[*]}" >> report2.log
	fi
done
