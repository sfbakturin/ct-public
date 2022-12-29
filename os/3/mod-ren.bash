#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

echo $$ > .pid

count=0

usr1()
{
	((count++))
	echo "You win! Current number of ${count}"
}

usr2()
{
	echo "You failed!"
	exit 0
}

term()
{
	echo "Exiting..."
	exit 0
}

trap 'usr1' USR1
trap 'usr2' USR2
trap 'term' SIGTERM

while true; do
	:
done
