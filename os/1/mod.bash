#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

cat table > table_copy
cat table_copy | awk '$2>74 {$2="PASS"; print $0} $2<75 {$2="NOT PASSED"; print $0}'> table
rm table_copy
