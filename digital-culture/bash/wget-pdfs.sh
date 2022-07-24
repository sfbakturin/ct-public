
#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

#!/bin/bash
wget -i wget-pdfs
i=0
c=0
for f in *
do
	type="$(file -b $f)"
	if [ $(head -c 4 "$f") = "%PDF" ] || [ "${type%%, *}" == "PDF document" ]; then
		j=$(wc -c < $f)
		((c=c+j))
		echo $f
	fi
	((i=i+1))
done
echo $c
