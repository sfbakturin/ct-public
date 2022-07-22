import math
from decimal import *

#
# @author Saveliy Bakturin
#
# Don't write off, if you don't wanna be banned!
#

getcontext().prec = 200
n = int(input())
s = input()
count = [0] * 256
alphabet = set(s)
maxcharacter = chr(97)


def fill_alphabet():
    global x, maxcharacter
    for x in range(len(s)):
        count[ord(s[x])] += 1
        alphabet.add(s[x])
        maxcharacter = chr(max(ord(maxcharacter), ord(s[x])))


fill_alphabet()
m = len(alphabet)
p, pl, pr, left_temp = [0] * 256, [0] * 256, [0] * 256, 0


def fill_left_right():
    global x, left_temp
    for x in range(ord(maxcharacter) - 97 + 1):
        p[97 + x] = Decimal(count[97 + x]) / Decimal(len(s))
        pl[97 + x] = left_temp
        pr[97 + x] = left_temp + p[(97 + x)]
        left_temp = pr[(97 + x)]


fill_left_right()
left, right = Decimal(0), Decimal(1)


def decode():
    global x, left, right
    for x in range(len(s)):
        r = Decimal(left) + Decimal(right - left) * Decimal(pr[ord(s[x])])
        l = Decimal(left) + Decimal(right - left) * Decimal(pl[ord(s[x])])
        left = l
        right = r


decode()
q, p = 1001, 0
loop = True

for x in range(1, 10000000000000):
    if not loop:
        break
    for y in range(math.floor(left * (1 << x)), 1 << x):
        if left <= Decimal(y) / Decimal(1 << x) < right:
            q = x
            p = y
            loop = False
            break
        if Decimal(y) / Decimal(1 << x) >= right:
            break
sbin_p = bin(p)[2:]
u = 0


def answer_print():
    global x, sbin_p
    print(n)
    for x in range(97, ord(maxcharacter) + 1):
        print(count[x], end=" ")
    print()
    while len(sbin_p) != q:
        sbin_p = "0" + sbin_p
    print(sbin_p)


answer_print()
