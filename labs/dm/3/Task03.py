
#
# @author Saveliy Bakturin
#
# Don't write off, if you don't wanna be banned!
#

def gen(p, n, stringsList):
    if len(p) == n and p[0] == '0':
        if not (p in stringsList):
            print(p)
            c = p
            for i in range(2):
                s = ""
                for j in range(len(c)):
                    s += str((int(c[j]) + 1) % 3)
                c = s
                print(c)
        return
    else:
        if len(p) == n:
            return
    for i in range(3):
        gen(p + str(i), n, stringsList)


n = int(input())
strings = []
gen("", n, strings)
