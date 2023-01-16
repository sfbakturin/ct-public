#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

while true
do
	top -b -c -n 1 > TABLE
	LINE=$(cat TABLE | grep "mem.bash")
	if [[ "$(cat TABLE | grep "mem.bash" | wc -l)" -eq "1" ]]
	then
		echo "$(echo $LINE | awk '{print $11}')" >> time.log
		echo "$(echo $LINE | awk '{print $5}')" >> virt.log
		echo "$(cat TABLE | grep "MiB Mem" | awk '{printf "%s %s\n", $6, $8}')" >> memo.log
		echo "$(cat TABLE | grep "MiB Swap" | awk '{printf "%s %s\n", $5, $7}')" >> swap.log
		echo "$(cat TABLE | sed -e '1,7d' | head -1 | tail +1 | awk '{print $12}')" >> prc1.log
		echo "$(cat TABLE | sed -e '1,7d' | head -2 | tail +2 | awk '{print $12}')" >> prc2.log
		echo "$(cat TABLE | sed -e '1,7d' | head -3 | tail +3 | awk '{print $12}')" >> prc3.log
		echo "$(cat TABLE | sed -e '1,7d' | head -4 | tail +4 | awk '{print $12}')" >> prc4.log
		echo "$(cat TABLE | sed -e '1,7d' | head -5 | tail +5 | awk '{print $12}')" >> prc5.log
	else
		rm TABLE
		exit 0
	fi
done
