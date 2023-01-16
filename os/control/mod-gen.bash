#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

win=$(shuf -i 0-1 -n 1)
line=""

while true
do
	read line
	if [ "$line" == "QUIT" ]
	then
		kill $(cat .pid)
		rm .pid
		echo "Exiting..."
		exit 0
	fi
	if [ "$line" == "$win" ]
	then
		kill -USR1 $(cat .pid)
		win=$(shuf -i 0-1 -n 1)
	else
		kill -USR2 $(cat .pid)
		rm .pid
		echo "Exiting..."
		exit 0
	fi
done
