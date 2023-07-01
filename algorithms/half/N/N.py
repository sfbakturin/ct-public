
#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

n, x, y = map(int, input().split())
n -= 1
time, printer_1, printer_2, count = min(x, y), 0, 0, 1
while count <= n:
	printer_1 += 1
	printer_2 += 1
	time += 1

	if printer_1 == x:
		count += 1
		printer_1 = 0

	if printer_2 == y:
		count += 1
		printer_2 = 0
print(time)
