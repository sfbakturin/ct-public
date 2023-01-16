#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

gcc -O4 -o gen gen.c
gcc -O4 -o data data.c

for ((i=1; i<=20; i++))
do
	./gen "file${i}"
done

bash parallel-loop20.bash
