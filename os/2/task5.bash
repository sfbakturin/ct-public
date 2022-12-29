#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

name="task4-data"
[[ ! -f $name ]] && bash task4.bash 1
for i in $(cat $name | awk '{print $2}' | uniq)
do
	total=0
	count=$(cat $name | awk -v counter="$i" '$2==counter {print $0}'| wc -l)
	for j in $(cat $name | awk -v counter="$i" '$2==counter {print $3}')
	do
		c=$j
		total=$(awk '{print $1+$2}' <<< "${total} ${c}")
	done
	res=$(awk '{print $1/$2}' <<< "${total} ${count}")
	cat $name | awk -v counter="$i" '$2==counter {printf "ProcessID=%s : Parent_ProcessID=%s : Average_Running_Time=%s\n", $1, $2, $3}' >> task5-solve
	echo "Average_Running_Children_of_ParentID=${i} is ${res}" >> task5-solve
done
rm $name
