#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

rm *.log

RIGHT=$(("56000000" / "10"))
LEFT="-1"
COUNT=$(wc -l FILE.log | awk '{print $1}')

while true
do
	[[ "$LEFT" -ge "$(("$RIGHT"-"1"))" ]] && break
	MIDDLE=$(($(("$LEFT" + "$RIGHT")) / "2"))
	./start.bash $MIDDLE
	LAST=$(wc -l <<< "$(ps -eo comm | grep "newmem.bash")")
	while (("$LAST" != "1"))
	do
		LAST=$(wc -l <<< "$(ps -eo comm | grep "newmem.bash")")
	done
	sudo dmesg | grep "newmem.bash" >> FILE.log
	if [[ "$(wc -l FILE.log | awk '{print $1}')" -eq "$COUNT" ]]
	then
		LEFT=$MIDDLE
	else
		COUNT=$(wc -l FILE.log | awk '{print $1}')
		RIGHT=$MIDDLE
	fi
done

echo $LEFT
