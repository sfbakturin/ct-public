#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

if [[ $PWD == $HOME ]];
then
	echo $HOME
	exit 0
else
	echo "ERROR"
	exit 1
fi
