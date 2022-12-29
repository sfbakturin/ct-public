#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

git reset --mixed dc6ad822df23ef5a51545de29a2a2d7ce1fed204
# or we can git rebase -i HEAD~2 and squash theme
git add .
git commit --amend
