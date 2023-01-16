#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

name="task4-data"
pids=$(ps aux | awk '$2!="PID" {print $2}')
for i in $pids
do
	[[ ! -d /proc/${i} ]] && continue
	ppids=$(cat /proc/${i}/status | awk '$1=="PPid:" {print $2}')
	sum=$(cat /proc/${i}/sched | awk '$1=="se.sum_exec_runtime" {print $3}')
	switches=$(cat /proc/${i}/sched | awk '$1=="nr_switches" {print $3}')
	art=$(awk '{print $1/$2}' <<< "${sum} ${switches}")
	echo -n "${i}"
	echo -n " "
	echo -n "${ppids}"
	echo -n " "
	echo "${art}"
done | sort -n -k 2 > $name

if [ "$#" -ne 1 ];
then
	cat $name | awk '{printf "ProcessID=%s : Parent_ProcessID=%s : Average_Running_Time=%s\n", $1, $2, $3}' > task4-solve
	rm $name
fi
