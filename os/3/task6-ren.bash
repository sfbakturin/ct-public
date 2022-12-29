#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

echo $$ > .pid

value=1
op=0

usr1()
{
	op=0
}

usr2()
{
	op=1
}

term()
{
	echo "Exiting..."
	exit 0
}

trap 'usr1' USR1
trap 'usr2' USR2
trap 'term' SIGTERM

while true
do
	case $op in
		0)
			value=$(awk '{print $1+$2}' <<< "${value} 2")
		;;
		1)
			value=$(awk '{print $1*$2}' <<< "${value} 2")
		;;
	esac
	echo $value
	sleep 1s
done
