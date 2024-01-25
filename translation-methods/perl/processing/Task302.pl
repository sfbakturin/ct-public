
#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

my(@lines);

while (<>)
{
	push(@lines, $_);
}

foreach (@lines)
{
	s/\<((.)*?)\>//g;
	s/((\s)((\s)+))/$2/g;
	s/(^(\s)+((.)*)$)/$3/;
	s/(^((.)*)(\s)+$)/$2/;
}

my($may_empty) = 0;
my(@lines_removed);

foreach (@lines)
{
	if (/^$/)
	{
		if ($may_empty == 1)
		{
			push(@lines_removed, $_);
			$may_empty = 0;
		}
	}
	else
	{
		push(@lines_removed, $_);
		$may_empty = 1;
	}
}

my($size) = scalar(@lines_removed);

for (my($i) = 0; $i < $size - 1; $i++)
{
	print($lines_removed[$i], "\n");
}

if (!($lines_removed[$size - 1] =~ /^$/))
{
	print($lines_removed[$size - 1], "\n");
}
