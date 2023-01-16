#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

awk '{if ($3=="(WW)") {$3="Warning:"; print $0}}' /var/log/anaconda/X.log > full.log
awk '{if ($3=="(II)") {$3="Information:"; print $0}}' /var/log/anaconda/X.log >> full.log
cat full.log
