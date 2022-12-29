#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

K=10
N=$(("56000000" / "10"))

for ((i=0; i<$K; i++))
do
	sleep 1s
	bash newmem.bash $N &
done
