#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

git stash
grep -v "THIS IS A BUG - remove the whole line to fix it." bug.txt > tmpfile && mv tmpfile bug.txt
git add .
git stash apply
echo "Finally, finished it!" >> bug.txt
git add .
git commit -m "done"
