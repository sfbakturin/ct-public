#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

DAT_SCR="$(date '+%Y%m%d')"
DAT_SCRF="$(date '+%Y-%m-%d')"
DAT_BKP=$DAT_SCRF
DIR_SRC="$HOME/source/"
DIR_REP="$HOME/backup-report"
DIR_DEST=""
FLG_COPY=1

if [[ ! -d "$DIR_SRC" ]]
then
	echo "No source directory was found."
	exit 1
fi

#copying function
function backup_copy()
{
	echo "===START NEW BACKUP===" >> $DIR_REP
	echo "New backup \"${DIR_DEST}\" from ${DAT_BKP}" >> $DIR_REP
	echo "List of files:" >> $DIR_REP
	find "$DIR_SRC" -type f >> $DIR_REP
	cp -r "$DIR_SRC/." "$DIR_DEST"
	echo "===END NEW BACKUP===" >> $DIR_REP
}

#merging function
function backup_merge()
{
	echo "===START MERGE BACKUP===" >> $DIR_REP
	echo "Merging backup \"${DIR_DEST}\" from ${DAT_SCRF}" >> $DIR_REP
	#find and add new files
	echo "List of new files:" >> $DIR_REP
	for el in $(find "$DIR_SRC" -type f)
	do
		FND=${el#"$HOME/source"}
		if [[ ! -f  "${DIR_DEST}/${FND}" ]]
		then
			echo "$el" >> $DIR_REP
			cp "$el" "${DIR_DEST}"
		fi
	done
	#find and merge old files
	echo "List of merged files:" >> $DIR_REP
	for el in $(find "$DIR_SRC" -type f)
	do
		FND=${el#"$HOME/source"}
		if [[ -f  "${DIR_DEST}/${FND}" ]]
		then
			CNT_NEW=$(wc -c < "$el")
			CNT_OLD=$(wc -c < "${DIR_DEST}/${FND}")
			if [[ "$CNT_NEW" -ne "$CNT_OLD" ]]
			then
				mv "${DIR_DEST}/${FND}" "${DIR_DEST}/${FND}.${DAT_SCRF}"
				echo "$el" >> $DIR_REP
				cp "${DIR_SRC}/${FND}" "${DIR_DEST}"
			fi
		fi
	done
	echo "===END MERGE BACKUP===" >> "$DIR_REP"
}

#find all backups
for fldr in $(ls $HOME)
do
	[[ -d $HOME/$fldr ]] && echo "$fldr"
done >> dirs

DIR_BKPS=$(cat dirs | grep -e "Backup-[0-9]\{4,\}-[0-9]\{2,\}-[0-9]\{2,\}")

rm dirs

#find <= 7 days last backup
for backup in $DIR_BKPS
do
	DAT_FND=$(echo $backup | awk -F '-' '{printf "%s%s%s", $2, $3, $4}')
	if [[ "$((($(date -d "$DAT_SCR" +%s) - $(date -d "$DAT_FND" +%s)) / 86400))" -le "7" ]]
	then
		DAT_BKP=$(date -d "$DAT_FND" '+%Y-%m-%d')
		FLG_COPY=0
		break
	fi
done

#if FLG_COPY==1, no <= 7 days backup was found, just copying
#else merge
if [[ "$FLG_COPY" -eq "1" ]]
then
	DIR_DEST=$HOME/"Backup-${DAT_BKP}"
	mkdir $DIR_DEST
	backup_copy
else
	DIR_DEST=$HOME/"Backup-${DAT_BKP}"
	backup_merge
fi
