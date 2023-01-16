#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

man bash | grep -o "[a-zA-Z]\{4,\}" | sort | uniq -c | sort -n -r -k 1 | head -n3 | awk '{print $2}'
