#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

MAX_LENGTH=$1
ARRAY=()
while true
do
	ARRAY+=(1 2 3 4 5 6 7 8 9 10)
	if [[ "${#ARRAY[*]}" -ge "$MAX_LENGTH" ]]
	then
		exit 0
	fi
done
