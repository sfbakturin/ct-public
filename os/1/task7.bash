#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

flag=0
touch emails.lst
touch emails_temp
for f in $(find /etc -type f)
do
	grep -a -o -e "[a-zA-Z0-9]\{1,\}@[a-zA-Z]\{1,\}\.[a-zA-Z]\{1,\}*" "$f" >> emails_temp
done
for f in $(cat emails_temp)
do
	if [[ $flag -eq 1 ]];
	then
		echo -n ", " >> emails.lst
	fi
	flag=1
	echo -n $f >> emails.lst
done
rm emails_temp
