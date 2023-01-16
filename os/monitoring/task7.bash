#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

start=$SECONDS
for i in $(ps aux | awk '{print $2}')
do
	[[ ! -d /proc/${i} ]] && continue
	cat /proc/${i}/io | awk -v num=$i '$1=="read_bytes:" {printf "%i %lu\n", num, $2}'
done | sort -n -k1 > temp1
sleep $((60 - start))
for i in $(ps aux | awk '{print $2}')
do
	[[ ! -d /proc/${i} ]] && continue
	cat /proc/${i}/io | awk -v num=$i '$1=="read_bytes:" {printf "%i %lu\n", num, $2}'
done | sort -n -k1 > temp2
lines="temp1"
while read -r i
do
	pid=$(awk '{print $1}' <<< $i)
	mem=$(awk '{print $2}' <<< $i)
	cat temp2 | awk -v num=$pid -v memory=$mem '$1==num {dif=$(($2-memory)); printf "%i %lu\n", $1, $dif}'
done <$lines | sort -n -r -k 2 | head -n 3 > temp3
lines="temp3"
while read -r i
do
	pid=$(awk '{print $1}' <<< $i)
	mem=$(awk '{print $2}' <<< $i)
	cmm=$(cat /proc/${pid}/cmdline)
	echo "${pid}:${mem}:${cmm}" >> out
done <$lines
cat out
rm temp1 temp2 temp3 out
s = $(cat /proc/$1/cmdline)
