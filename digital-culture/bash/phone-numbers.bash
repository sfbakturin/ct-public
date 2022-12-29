#!/bin/bash

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

i=0
for s in $(cat phone-numbers);
do
	l=0
	x=0
	p=0
	z=0
	for (( j=0; j<${#s}; j++ )); do
		c=${s:$j:1}
		if [[ $c == '+' ]] && [[ $j == 0 ]]; then
			((p=1))
			((l=1))
		fi
		if [[ $c == '0' ]] && [[ $j == 0 ]]; then
			((z=1))
		fi
		if [[ $c == '0' ]] && [[ $j == 1 ]] && [[ ${s:$j - 1:1} == '+' ]]; then
			((z=1))
		fi
		if [[ $c == '0' ]] || [[ $c == '1' ]] || [[ $c == '2' ]] || [[ $c == '3' ]] || [[ $c == '4' ]] || [[ $c == '5' ]] || [[ $c == '6' ]] || [[ $c == '7' ]] || [[ $c == '8' ]] || [[ $c == '9' ]]; then
			((l=l+1))
		else
			if [[ $c == '+' ]] && [[ $j == 0 ]]; then
				((x=x+0))
			else
				((x=1))
			fi
		fi
	done
	if  [[ $p == 0 ]] && [[ $l -le 15 ]] && [[ $l > 1 ]] && [[ $x == 0 ]] && [[ $z == 0 ]]; then
		((i=i+1))
	fi
	q=$(($l-1))
	if [[ $p == 1 ]] && [[ $q -le 15 ]] && [[ $q > 1 ]] && [[ $x == 0 ]] && [[ $z == 0 ]]; then
		((i=i+1))
	fi
done
echo $i
