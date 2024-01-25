
#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

while (<>)
{
	s/(\(((.)*?)\))/()/g;
	print;
}