#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

if [[ "$#" -eq "0" ]]
then
	echo "You must give name of file to restore from trash."
	exit 1
fi

if [[ "$#" -gt "1" ]]
then
	echo "Warning: the program will restore from trash only the first argument."
fi

echo $$ > .pid

FILENAME="$1"

bash trash-checker.bash

while read -r i
do
	[[ "${#i}" -eq "0" ]] && break
	found=$(cut -c2- <<< "$(awk '{first=$1; $1=""; print $0; }' <<< "$i")")
	echo "Do you want to recover ${found}?"
	echo "\"Y\" or \"y\" to recover. Default - continue."
	read line </dev/tty
	if [[ $line == "Y" || $line == "y" ]]
	then
		FILEPATH=${found%"/${FILENAME}"}
		if [[ ! -d $FILEPATH ]]
		then
			echo "Warning: the path ${FILEPATH} is no longer exist. File will be recovered to home directory."
			FILEPATH=$HOME
		fi
		if [[ -f "$FILEPATH/$FILENAME" ]]
		then
			echo "Warning: in the ${FILEPATH} there is exists file named \""${FILENAME}"\"."
			while (true)
			do
				echo "Please, rename file."
				read FILENAME </dev/tty
				[[ ! -f "$FILEPATH/$FILENAME" ]] && break
			done
		fi
		FILENAMESHA=$(cat ~/.trash.log | grep "${found}$" | awk '{print $1}')
		ln -P ~/.trash/"$FILENAMESHA" "$FILEPATH"
		rm ~/.trash/"$FILENAMESHA"
		mv "$FILEPATH"/"$FILENAMESHA" "$FILEPATH"/"$FILENAME"
		sed "/i/d" ~/.trash.log > ~/.trash.log2 && mv ~/.trash.log2 ~/.trash.log
		exit 0
	fi
	echo ""
done <<< $(cat ~/.trash.log | grep "/${FILENAME}$")

echo "No such file was found in trash."
