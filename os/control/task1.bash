#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

mkdir $HOME/test && echo "catalog test was created successfully" > ~/report && touch $HOME/test/$(date '+%d-%m-%Y_%H-%M-%S')
ping www.net_nikogo.net || echo "$(date '+%d.%m.%Y %H:%M:%S') Name of wervice not found, while try to ping www.net_nikogo.net" >> ~/report
