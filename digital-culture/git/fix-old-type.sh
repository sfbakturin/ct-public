
#
# @author Saveliy Bakturin
#
# Don't write off, if you don't wanna be banned!
#

#!/bin/bash
git rebase -i 98e6b86bbed506a2225aec8f1bc9791b88e5ba8f
# or we can git rebase -i HEAD~2 and select edit where there is error
echo "Hello world" > file.txt
git add .
git rebase --continue
echo "Hello world" > file.txt
echo "Hello world is an excellent program." >> file.txt
git add .
git rebase --continue
