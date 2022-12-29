#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

git bisect start
mv -v faulty-check faulty-check.bash
chmod u+x faulty-check.bash
git bisect good
git bisect bad 1.0
git bisect run ./faulty-check.bash
# human hands answers
