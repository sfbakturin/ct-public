
#
# @author Saveliy Bakturin
# <p>
# Don't write off, if you don't wanna be banned!
#

n = int(input())
mas, maximum = [], -1
for i in range (0, n):
	user = int(input())
	mas.append(user)
	maximum = max(maximum, user)
primes = []
mp = [-1 for i in range(0, maximum + 1)]
for i in range(2, maximum + 1):
	if (mp[i] == -1):
		primes.append(i)
		mp[i] = i
	for p in primes:
		if (p <= mp[i] and i * p <= maximum):
			mp[i * p] = p
		else:
			break
for i in range(0, n):
    print("YES" if mp[mas[i]] == mas[i] else "NO")
