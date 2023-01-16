#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

cat /etc/passwd > users
cat users | tr ':' ' ' | sort -n -k 3 | awk '{print $1}'
rm users
