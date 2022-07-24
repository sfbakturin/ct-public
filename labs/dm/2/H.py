import sys
from decimal import *

#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

getcontext().prec = 2000
n = int(input())
c = list(map(int, sys.stdin.readline().split()))
sum_c = sum(c)
s = input()
lc = ["0"] * (n + 1)
p, ll, lr, lc = [Decimal(0)] * (n + 1), [Decimal(0)] * (n + 1), [Decimal(0)] * (n + 1), ["0"] * (n + 1)
left_temp = Decimal(0)


def fill_left_right():
	global x, left_temp
	for x in range(n):
		p[x] = Decimal(c[x]) / Decimal(sum_c)
		ll[x] = Decimal(left_temp)
		lr[x] = Decimal(left_temp) + Decimal(p[x])
		left_temp = Decimal(left_temp) + Decimal(p[x])
		lc[x] = chr(97 + x)


fill_left_right()

code = Decimal(int(s, 2)) / Decimal(2 ** len(s))
answer = ""
for x in range(sum_c):
	for y in range(n):
		if ll[y] <= code < lr[y]:
			edge_left = Decimal(ll[y])
			edge_right = Decimal(lr[y])
			answer += lc[y]
			ll[0] = edge_left
			for z in range(n):
				if z != 0:
					ll[z] = lr[z - 1]
				lr[z] = Decimal(ll[z]) + Decimal((edge_right - edge_left) * (p[z]))
			lr[n - 1] = Decimal(edge_right)
			break
print(answer)
