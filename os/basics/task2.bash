#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

cur=""
res=""
bad="q"

while true
do
	read cur
	if [[ "$cur" == "$bad" ]]
	then break
	fi
	res+="$cur"
done
echo "$res"
