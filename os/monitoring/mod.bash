#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

for i in $(ps aux | tail -n+2 | awk '{print $2}')
do
	[[ ! -d /proc/${i} ]] && continue
	mem=$(cat /proc/${i}/stat | awk '{print $23}')
	pid=$(ps aux | awk -v num=$i '$2==num {print $1}')
	cat <<< "${pid} ${mem}"
done | sort -n -r -k2 | head -n1 | awk '{printf "Maximum memory from %s is %lu\n", $1, $2}'

muc=$(ps aux | tail -n+2 | awk '{print $1}' | sort | uniq -c | sort -n -r -k1 | head -n1 | awk '{print $2}')
tot=0
for i in $(ps aux | tail -n+2 | awk -v u=$muc '$1==u {print $2}')
do
	[[ ! -d /proc/${i} ]] && continue
	mem=$(cat /proc/${i}/stat | awk '{print $23}')
	tot=$(awk '{print $1+$2}' <<< "${tot} ${mem}")
done
echo "Summary memory from ${muc} that born many processes is ${tot}"
