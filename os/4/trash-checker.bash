#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

function clear()
{
	kill $(cat .pid)
	rm .pid
}

if [[ ! -d ~/.trash && ! -f ~/.trash.log ]]
then
	mkdir ~/.trash
	touch ~/.trash.log
	exit 0
fi

if [[ ! -d ~/.trash && -f ~/.trash.log ]]
then
	echo "Invalid trash: no directory was found."
	clear
	exit 1
fi

if [[ -d ~/.trash && ! -f ~/.trash.log ]]
then
	echo "Invalid trash: no log file was found."
	clear
	exit 1
fi

if [[ -d ~/.trash && -f ~/.trash.log ]]
then
	while read -r i
	do
		[[ "${#i}" -eq "0" ]] && break
		file=$(cut -c2- <<< "$(awk '{first=$1; $1=""; print $0; }' <<< "$i")")
		sum=$(awk '{print $1}' <<< "$(sha256sum -t <<< "$file")")
		if [[ ! -f ~/.trash/$(awk '{print $1}' <<< "$(sha256sum -t <<< "$file")") ]]
		then
			echo "Invalid trash: no ${file} was found in trash."
			clear
			exit 1
		fi
	done <<< $(cat ~/.trash.log)
fi
