
#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

while (<>)
{
	s/(((a)((.)*?)(a)){3})/bad/g;
	print;
}