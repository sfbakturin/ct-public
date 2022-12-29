#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

value=1
op=0
line=""

(tail -f pipe) |
while true
do
	read line
	if [ "$line" == "QUIT" ]
	then
		echo "Exiting..."
		kill $(cat .pid)
		rm .pid
		killall tail
		exit 0
	fi
	if [ "$line" == "*" ]
	then
		op=1
		echo "status: multiply"
		continue
	fi
	if [ "$line" == "+" ]
	then
		op=0
		echo "status: sum"
		continue
	fi
	if [[ "$line" -eq "$line" ]]
	then
		[[ "$op" -eq "0" ]] && value=$(awk '{print $1+$2}' <<< "${value} ${line}")
		[[ "$op" -eq "1" ]] && value=$(awk '{print $1*$2}' <<< "${value} ${line}")
	else
		echo "ERROR: it's not an integer!"
		kill $(cat .pid)
		rm .pid
		killall tail
		exit 1
	fi
	echo $value
done
