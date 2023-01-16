#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

for i in $(ps aux | tail -n+2 | awk '{print $2}')
do
	[[ ! -d /proc/${i} ]] && continue
	cat /proc/${i}/stat | awk '{printf "%i %lu\n", $1, $23}'
done | sort -n -k2 -r | head -n1 | awk '{print $1}'
