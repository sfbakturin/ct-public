
#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

#!/bin/bash
unzip find-exe.zip
mkdir FOUND
find . -executable -type f | xargs cp -t FOUND
cd FOUND
find . -executable -type f -printf '%P\n'
