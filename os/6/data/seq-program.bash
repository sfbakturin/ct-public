#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

N=$1

for ((i=1; i<=$N; i++))
do
	./data "file${i}"
done
