#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

DAT_MIN=$(date '+%Y-%m-%d')

[[ -d "$HOME/restore/" ]] && rm -rf "$HOME/restore"
mkdir "$HOME/restore"

#find all backups
for fldr in $(ls $HOME)
do
	[[ -d $HOME/$fldr ]] && echo "$fldr"
done >> dirs

DIR_BKPS=$(cat dirs | grep -e "Backup-[0-9]\{4,\}-[0-9]\{2,\}-[0-9]\{2,\}")

rm dirs

if [[ "${#DIR_BKPS}" -eq "0" ]]
then
	echo "No exist backup was found."
	exit 1
fi

#find min backup
for backup in $DIR_BKPS
do
	DAT_FND=$(echo $backup | awk -F '-' '{printf "%s-%s-%s", $2, $3, $4}')
	if [[ "$(date -d "$DAT_MIN" +%s)" -ge "$(date -d "$DAT_FND" +%s)" ]]
	then
		DAT_MIN=$DAT_FND
	fi
done

BKP_CUR="$HOME/Backup-${DAT_MIN}"
cp -r "$BKP_CUR/." "$HOME/restore"
find "$HOME/restore/" -type f -regextype posix-extended -regex '.*.[0-9]{4}-[0-9]{2}-[0-9]{2}' -exec rm -r {} \;
