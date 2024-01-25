
#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

while (<>)
{
	s/(\b((\w)(\w)((\w)*))\b)/$4$3$5/g;
	print;
}