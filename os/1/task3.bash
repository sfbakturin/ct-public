#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

while true
do
	user=0

	echo "1) nano"
	echo "2) vi"
	echo "3) links"
	echo "4) exit"

	read user

	case "$user" in
		1 ) nano;;
		2 ) vi;;
		3 ) links;;
		4 ) exit 0;;
	esac
done
