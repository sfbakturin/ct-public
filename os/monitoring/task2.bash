#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

ps aux | awk '$11 ~ /^\/sbin\// {print $2}' > task2-solve
