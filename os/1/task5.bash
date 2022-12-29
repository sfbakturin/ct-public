#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

awk '$2=="INFO" {print $0}' /var/log/anaconda/syslog > info.log
