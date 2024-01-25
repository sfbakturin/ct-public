
#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

while (<>)
{
	s/^((\W)*)(\b((\w)*)\b)((\W)*)(\b((\w)*)\b)((.)*)$/$1$8$6$3$11/;
	print;
}