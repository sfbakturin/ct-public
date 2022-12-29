#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

TIMEFORMAT=%R

N=$1

for ((i=1; i<=10; i++))
do
	time bash seq-program.bash $N
done
