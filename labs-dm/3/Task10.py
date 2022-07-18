
#
# @author Saveliy Bakturin
#
# Don't write off, if you don't wanna be banned!
#

def compare(item1, item2):
    if len(item1) < len(item2):
        return -1
    if len(item1) > len(item2):
        return 1
    return 0


def gen(n, sum, strings, operator):
    if sum == n:
        s = ""
        for x in range(len(operator) - 1):
            s += str(operator[x])
            s += "+"
        s += str(operator[len(operator) - 1])
        strings.append(s)
        return
    for i in range(1, n + 1):
        if len(operator) > 0:
            if sum + i <= n and operator[len(operator) - 1] <= i:
                a = operator.copy()
                a.append(i)
                gen(n, sum + i, strings, a)
        else:
            a = operator.copy()
            a.append(i)
            gen(n, sum + i, strings, a)


n = int(input())
strings, operator = [], []
gen(n, 0, strings, operator)
strings.sort(key=lambda x: len(x), reverse=True)
for x in strings:
    print(x)
