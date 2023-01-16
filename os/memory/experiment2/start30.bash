#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

K=30
N=5600000

for ((i=0; i<$K; i++))
do
	sleep 1s
	bash newmem.bash $N &
done
