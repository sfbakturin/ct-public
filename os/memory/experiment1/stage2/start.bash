#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

rm *.log

bash mem.bash &
bash mem2.bash &
sleep 1s
bash info.bash
