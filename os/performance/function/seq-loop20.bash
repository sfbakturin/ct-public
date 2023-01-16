#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

TIMEFORMAT=%R

for ((i=1; i<=20; i++))
do
	time bash seq-loop10.bash $i
done
