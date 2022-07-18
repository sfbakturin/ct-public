
#
# @author Saveliy Bakturin
#
# Don't write off, if you don't wanna be banned!
#

#!/bin/bash
unzip mod-sort.zip -d FOUND
cd FOUND
ls -tr > SORTEDLIST
touch CONCATENATING
for f in $(cat SORTEDLIST);
do
    echo $f
    if [ $f != "SORTEDLIST" ] && [ $f != "CONCATENATING" ]; then
        cat $f >> CONCATENATING
    fi
done
sha256sum CONCATENATING
