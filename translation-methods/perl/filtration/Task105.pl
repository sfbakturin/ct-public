
#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

while (<>)
{
	print if /((x|y|z)((.){5,17})(x|y|z))/;
}