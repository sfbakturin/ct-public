
#
# @author Saveliy Bakturin
#
# Don't write off, if you don't wanna be banned!
#

class Stack:
    def __init__(self):
        self.items = []

    def empty(self):
        return self.items == []

    def push(self, item):
        self.items.append(item)

    def pop(self):
        return self.items.pop()

    def peek(self):
        return self.items[len(self.items) - 1]

    def size(self):
        return len(self.items)


n = int(input())
answer, counter, leftStackValue, rightStackValue, leftStackMin, rightStackMin = [], 0, Stack(), Stack(), Stack(), Stack()
for i in range(n):
    inputList = input().split()
    if inputList[0] == "+":
        temp = int(inputList[1])
        if leftStackValue.empty():
            temp = temp
        else:
            temp = min(temp, leftStackMin.peek())
        leftStackValue.push(int(inputList[1]))
        leftStackMin.push(temp)
        counter += 1
        if not(rightStackValue.empty()) and not(leftStackValue.empty()):
            temp = min(leftStackMin.peek(), rightStackMin.peek())
        else:
            if rightStackValue.empty():
                temp = leftStackMin.peek()
            else:
                temp = rightStackMin.peek()
        answer.append(temp)
    else:
        if counter == 0:
            answer.append(-1)
        else:
            if rightStackValue.empty():
                while not(leftStackValue.empty()):
                    temp_min = 0
                    temp_value = leftStackValue.peek()
                    leftStackValue.pop()
                    if rightStackValue.empty():
                        temp_min = temp_value
                    else:
                        temp_min = min(temp_value, rightStackMin.peek())
                    rightStackValue.push(temp_value)
                    rightStackMin.push(temp_min)
            rightStackValue.pop()
            rightStackMin.pop()
            counter -= 1
            if counter == 0:
                answer.append(-1)
            else:
                if not(rightStackValue.empty()) and not(leftStackValue.empty()):
                    temp = min(leftStackMin.peek(), rightStackMin.peek())
                else:
                    if rightStackValue.empty():
                        temp = leftStackMin.peek()
                    else:
                        temp = rightStackMin.peek()
                answer.append(temp)
for i in answer:
    print(i)
