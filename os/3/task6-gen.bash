#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

line=""

while true
do
	read line
	if [ "$line" == "TERM" ]
	then
		echo "Exiting..."
		kill $(cat .pid)
		rm -rf .pid
		exit 0
	fi
	if [ "$line" == "*" ]
	then
		kill -USR2 $(cat .pid)
	fi
	if [ "$line" == "+" ]
	then
		kill -USR1 $(cat .pid)
	fi
done
