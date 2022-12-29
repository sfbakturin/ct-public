#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

bash task4-util.bash &
first="$!"
bash task4-util.bash &
bash task4-util.bash &
third="$!"

cpulimit -p $first -l 10 &
kill $third
