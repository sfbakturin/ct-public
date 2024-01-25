
#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

while (<>)
{
	print if /(\b((\w+)\g3)\b)/;
}