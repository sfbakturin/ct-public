
#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

#!/bin/bash
unzip word-count.zip
s=$(cat target.word)
cd _zmidltJpWE
mkdir FOUND
find . -type f -name "*$s*" | xargs cp -t FOUND
cd FOUND
find ./ -type f -exec wc -w {} +
