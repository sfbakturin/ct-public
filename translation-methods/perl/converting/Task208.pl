
#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

while (<>)
{
	s/(\b((\d+)(0))\b)/$3/g;
	print;
}