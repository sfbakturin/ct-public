#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

ps aux | awk '$1=="user" {print $0}' | wc -l > task1-solve
ps aux | awk '$1=="user" {printf "%i:%s\n", $2, $11}' >> task1-solve
