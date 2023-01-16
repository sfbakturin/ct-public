#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

if [[ "$#" -eq "0" ]]
then
	echo "You must give name of file to delete."
	exit 1
fi
if [[ "$#" -gt "1" ]]
then
	echo "Warning: the program will delete only the first argument."
fi

echo $$ > .pid

FILENAME="$1"
PATHFILE=$(readlink -f "$FILENAME")
FILENAMESHA=$(awk '{print $1}' <<< "$(sha256sum -t <<< "$PATHFILE"))")

bash trash-checker.bash

if [[ ! -f "$FILENAME" ]]
then
	echo "No ${FILENAME} was found."
	rm -rf .pid
	exit 1
fi

if [[ ! -f ~/.trash/"$FILENAMESHA" ]]
then
	ln -P "$FILENAME" ~/.trash
	rm "$FILENAME"
	mv ~/.trash/"$FILENAME" ~/.trash/"$FILENAMESHA"
	echo "${FILENAMESHA} ${PATHFILE}" >> ~/.trash.log
	rm .pid
else
	echo "Deleting this file will cause a collision and cleanup of the previous file with the same name. Rename the file."
	rm .pid
	exit 1
fi
