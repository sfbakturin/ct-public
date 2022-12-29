#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

echo $$ > .pid

line=""

while true
do
	read line
	echo "$line" > pipe
done
