class Stack:
    def __init__(self):
        self.items = []

    def push(self, item):
        self.items.append(item)

    def peek(self):
        return self.items[-1]

    def pop(self):
        return self.items.pop()

    def is_empty(self):
        return (self.items == [])

    def size(self):
        return len(self.items)

    def printStack(self):
        x = []
        for items in reversed(self.items):
            x.append(items)
        return x


def isNum(value):
    try:
        value = int(value)
    except ValueError:
        return False
    else:
        return True

def isfloat(value):
  try:
    return float(value) and '.' in value
  except ValueError:
    return False

def isbool(value):
    if value == ":true:" or value == ":false:":
        return True
    else:
        return False

def isString(value):
    if len(value) > 2:
        if (ord (value[0]) > 127 or ord (value[0]) < 0) and (ord (value[len(value) -1]) > 127 or ord (value[len(value) -1]) < 0):
            return True;
        else:
            return False
    else:
        return False

def isPushString(value):
    if len(value) > 5:
        value = value[5:]
        if value[0] == '\"' and value[len(value)-1] == '\"':
            return True
        else:
            return False
    else:
        return False

def inDic(dic , value):
    ans = False
    for v in dic.keys():
        if v == value:
            ans = True
    return ans

def removeQuotes(input):
    i = 0
    for line in input:
        if line[0] == '\"':
            line.strip("\"")
            input[i] = line.strip("\"")
        i += 1

    return input

def getlet(inputList):
    lol = []
    forIndex = 0
    for line in inputList:
        if line == "let":

            letList = []
            letCount = 0
            i = forIndex
            while i < len(inputList)-1:
                whileInput = inputList[i + 1]
                if whileInput == "let":
                    letList.append(whileInput)
                    letCount +=1
                elif whileInput == "end" and letCount == 0:
                    lol.append(letList)
                    break
                elif whileInput == "end":
                    letList.append(whileInput)
                    letCount -=1
                else:
                    letList.append(whileInput)

                i += 1

        forIndex +=1
    return lol

def getfun(inputList):
    outputList = []
    funCount = 0
    # print (len(inputList))
    for line in inputList:
        # print (line + " " + str(funCount))
        if len(line) > 5:
            if line[:4] == "fun " or line[:8] == "inOutFun":
                outputList.append(line)
                funCount += 1
            elif line == "funEnd" and funCount - 1 == 0:
                return outputList[1:]
            elif line == "funEnd":
                outputList.append(line)
                funCount -= 1
            else:
                outputList.append(line)
        else:
            outputList.append(line)

    return outputList[1:]

def evalfun(inputList, argument):
    s = Stack()

    isinOutFun = False

    if inputList[len(inputList) - 1] == "sai":
        dic = inputList[len(inputList) - 3]
        orgArg = inputList[len(inputList) - 2]
        dic[inputList[len(inputList) - 2]] = argument
        inputList = inputList[:-3]
        isinOutFun = True
    else:
        dic = inputList[len(inputList) - 2]
        orgArg = inputList[len(inputList) - 1]
        dic[inputList[len(inputList) - 1]] = argument
        inputList = inputList[:-2]

    letCount = 0
    i = 0

    while i < len(inputList):
        line = inputList[i]

        lineArray = line.split()
        command = lineArray[0]

        if len(lineArray) > 2 and isPushString(line):
            line = line[5:]
            value = line
        elif len(lineArray) == 3 and (command == "fun" or command == "inOutFun"):
            funName = lineArray[1]
            arg = lineArray[2]
        elif len(lineArray) == 2:
            value = lineArray[1]
        elif len(lineArray) == 1:
            command = command

        if command == "quit":
            break

        elif command == "push":

            if value == "-0":
                s.push(0)

            elif inDic(dic, value):
                s.push(value)

            elif isString(value):  # check if it is a string
                if value[1:len(value) - 1].isalnum:
                    s.push(value[1:len(value) - 1])
                else:
                    s.push(":error:")

            elif value[0] == '\"':  # check if it is a string
                # if value.strip("\"").isalnum:
                #     s.push(value.strip("\""))
                # else:
                #     s.push(":error:")
                s.push(value)

            elif isNum(value):
                s.push(value)

            elif value.isalnum():  # check if it is a name
                s.push(value)
                dic.update({value: None})

            elif isfloat(value):
                s.push(":error:")

            else:
                s.push(":error:")

        elif command == "pop":
            if s.is_empty():
                s.push(":error:")
            else:
                s.pop()

        elif command == ":true:":
            s.push(":true:")

        elif command == ":false:":
            s.push(":false:")

        elif command == "sub":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())

                if inDic(dic, y) and inDic(dic, x):
                    tempY = y
                    tempX = x
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y - x))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y - x))
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y - x))
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y):
                    x = int(x)
                    y = int(y)
                    s.push(str(y - x))
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "add":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y + x))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y + x))
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y + x))
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y):
                    x = int(x)
                    y = int(y)
                    s.push(str(y + x))
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "mul":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y * x)))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y * x)))
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y * x)))
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y):
                    x = int(x)
                    y = int(y)
                    s.push(str(int(y * x)))
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "div":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())

                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y / x)))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y / x)))
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y / x)))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y) and not x == "0":
                    x = int(x)
                    y = int(y)
                    s.push(str(int(y / x)))
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "rem":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())

                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y % x)))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y % x)))
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y % x)))
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y) and not x == "0":
                    x = int(x)
                    y = int(y)
                    s.push(str(int(y % x)))
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "neg":
            if s.size() > 0:
                x = str(s.pop())
                if inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x):
                        s.push(str(int(x) * -1))
                    else:
                        s.push(tempX)
                        s.push(":error:")
                elif isNum(x):
                    s.push(str(int(x) * -1))
                else:
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "swap":
            if s.size() > 1:
                x = s.pop()
                y = s.pop()
                s.push(x)
                s.push(y)
            else:
                s.push(":error:")

        elif command == ":error:":
            s.push(":error:")

        elif command == "and":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isbool(x) and isbool(y):
                        if x == ":true:" and y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isbool(x) and isbool(y):
                        if x == ":true:" and y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isbool(x) and isbool(y):
                        if x == ":true:" and y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isbool(x) and not inDic(dic, y) and isbool(y):
                    if x == ":true:" and y == ":true:":
                        s.push(":true:")
                    else:
                        s.push(":false:")
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "or":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isbool(x) and isbool(y):
                        if x == ":true:" or y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isbool(x) and isbool(y):
                        if x == ":true:" or y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isbool(x) and isbool(y):
                        if x == ":true:" or y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isbool(x) and not inDic(dic, y) and isbool(y):
                    if x == ":true:" or y == ":true:":
                        s.push(":true:")
                    else:
                        s.push(":false:")
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "not":
            if s.size() > 0:
                tempX = x
                x = str(s.pop())
                if inDic(dic, x):
                    x = str(dic[x])
                    if isbool(x):
                        if x == ":true:":
                            s.push(":false:")
                        else:
                            s.push(":true:")
                    else:
                        s.push(tempX)
                        s.push(":error:")
                elif isbool(x):
                    if x == ":true:":
                        s.push(":false:")
                    else:
                        s.push(":true:")
                else:
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "equal":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        if x == y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y):
                        if x == y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        if x == y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y):
                    if x == y:
                        s.push(":true:")
                    else:
                        s.push(":false:")
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "lessThan":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        if x > y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y):
                        if x > y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        if x > y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y):
                    if x > y:
                        s.push(":true:")
                    else:
                        s.push(":false:")
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "bind":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    if dic[x] == None:
                        s.push(y)
                        s.push(x)
                        s.push(":error:")
                    else:
                        dic[y] = dic[x]
                        s.push(":unit:")
                elif inDic(dic, y) and not x == ":error:":
                    dic[y] = x
                    s.push(":unit:")
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")

                if (i == len(inputList) - 1 and isinOutFun):
                    return (x + "#")
                elif (i == len(inputList) - 2 and inputList[i + 1] == "return" and isinOutFun):
                    return (x + "#")

            else:
                s.push(":error:")

        elif command == "if":
            if s.size() > 2:
                x = str(s.pop())
                y = str(s.pop())
                z = str(s.pop())
                if inDic(dic, z):
                    tempZ = z
                    z = str(dic[z])
                    if isbool(z):
                        if z == ":true:":
                            s.push(x)
                        else:
                            s.push(y)
                    else:
                        s.push(tempZ)
                        s.push(y)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, z) and isbool(z):
                    if isbool(z):
                        if z == ":true:":
                            s.push(x)
                        else:
                            s.push(y)
                    else:
                        s.push(z)
                        s.push(y)
                        s.push(x)
                        s.push(":error:")
                else:
                    s.push(z)
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "let":

            letLol = getlet(inputList)
            letLol[0].append("end")
            letLol[0].insert(0, "let")
            tempDic = dic.copy()
            ans = letEnd(letLol[letCount], dic)
            dic = tempDic
            s.push(ans[0])

            i = i + len(letLol[letCount]) - 1

            letCount += 1

        elif command == "end":
            s = s

        elif command == "fun":
            funName = lineArray[1]
            arg = lineArray[2]
            funArray = getfun(inputList[i:])

            dic[funName] = funArray
            tempDic = dic.copy()
            funArray.append(dic)
            dic = tempDic
            funArray.append(arg)

            s.push(":unit:")

            i = i + len(funArray) - 1

        elif command == "funEnd":
            s = s

        elif command == "call":
            if s.size() > 1:
                funct = str(s.pop())
                argu = str(s.pop())
                if inDic(dic, argu) and inDic(dic, funct):
                    if isinstance(dic[funct], list):
                        tempDic = dic.copy()
                        if str(evalfun(dic[funct], dic[argu])[-1:]) == "$":
                            s.push(evalfun(dic[funct], dic[argu])[:-1])
                            value = evalfun(dic[funct], dic[argu])[:-1]
                            tempDic[argu] = value
                        elif str(evalfun(dic[funct], dic[argu])[-1:]) == "#":
                            value = evalfun(dic[funct], dic[argu])[:-1]
                            tempDic[argu] = value
                        else:
                            s.push(evalfun(dic[funct], dic[argu]))

                        dic = tempDic
                    else:
                        s.push(argu)
                        s.push(funct)
                        s.push(":error:")
                elif inDic(dic, funct) and argu == ":error:":
                    s.push(argu)
                    s.push(funct)
                    s.push(":error:")
                elif inDic(dic, funct) and not inDic(dic, argu):
                    if isinstance(dic[funct], list):
                        tempDic = dic.copy()
                        if str(evalfun(dic[funct], argu)[-1:]) == "$":
                            s.push(evalfun(dic[funct], argu)[:-1])
                            value = evalfun(dic[funct], argu)[:-1]

                            tempDic[argu] = value
                        elif str(evalfun(dic[funct], argu)[-1:]) == "#":
                            value = evalfun(dic[funct], argu)[:-1]
                            dic[argu] = value
                        else:
                            s.push(evalfun(dic[funct], argu))
                        dic = tempDic
                    else:
                        s.push(argu)
                        s.push(funct)
                        s.push(":error:")
                else:
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "return":
            if s.size() > 0:
                if isinOutFun:
                    x = str(s.pop())
                    if x == orgArg:
                        if inDic(dic, x):
                            return str(dic[x]) + "$"
                        else:
                            return str(x)
                    else:
                        if inDic(dic, x):
                            return str(dic[x])
                        else:
                            return str(x)
                else:
                    x = str(s.pop())
                    if inDic(dic, x):
                        if isinstance(dic[x], list):
                            return str(x)
                        else:
                            return str(dic[x])
                    else:
                        return str(x)
            else:
                return ":error:"

        elif command == "inOutFun":
            funName = lineArray[1]
            arg = lineArray[2]
            funArray = getfun(inputList[i:])

            dic[funName] = funArray
            tempDic = dic.copy()
            funArray.append(dic)
            dic = tempDic
            funArray.append(arg)
            funArray.append("sai")

            s.push(":unit:")

            i = i + len(funArray) - 2

        else:
            s.push(":error:")


        i += 1


        x = s.printStack()
        # x = removeQuotes(x)

        # print (x)
        # print (dic)

def letEnd(input, dic):
    i = 0
    letCount = 0
    ans = []
    s = Stack()
    while i < len(input):
        line = input[i]
        lineArray = line.split()
        command = lineArray[0]

        if len(lineArray) > 2 and isPushString(line):
            line = line[5:]
            value = line
        elif len(lineArray) == 3 and (command == "fun" or command == "inOutFun"):
            funName = lineArray[1]
            arg = lineArray[2]
        elif len(lineArray) == 2:
            value = lineArray[1]
        elif len(lineArray) == 1:
            command = command

        if command == "quit":
            break

        elif command == "push":
            if value == "-0":
                s.push(0)

            elif inDic(dic, value):
                s.push(value)
                if i == len(input) - 1:
                    ans.append(value)
                    return ans

            elif isString(value):  # check if it is a string
                s.push(value[1:len(value) - 1])

            elif value[0] == "\"":  # check if it is a string
                s.push(value.strip("\""))

            elif isNum(value):
                s.push(value)

            elif value.isalnum():  # check if it is a name
                s.push(value)
                dic.update({value: None})

            elif isfloat(value):
                s.push(":error:")

            else:
                s.push(":error:")

        elif command == "pop":
            if s.is_empty():
                s.push(":error:")
            else:
                s.pop()

        elif command == ":true:":
            s.push(":true:")

        elif command == ":false:":
            s.push(":false:")

        elif command == "sub":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())

                if inDic(dic, y) and inDic(dic, x):
                    tempY = y
                    tempX = x
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y - x))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y - x))
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y - x))
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y):
                    x = int(x)
                    y = int(y)
                    s.push(str(y - x))
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "add":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y + x))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y + x))
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y + x))
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y):
                    x = int(x)
                    y = int(y)
                    s.push(str(y + x))
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "mul":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y * x)))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y * x)))
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y * x)))
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y):
                    x = int(x)
                    y = int(y)
                    s.push(str(int(y * x)))
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "div":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())

                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y / x)))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y / x)))
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y / x)))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y) and not x == "0":
                    x = int(x)
                    y = int(y)
                    s.push(str(int(y / x)))
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "rem":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())

                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y % x)))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y % x)))
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y % x)))
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y) and not x == "0":
                    x = int(x)
                    y = int(y)
                    s.push(str(int(y % x)))
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "neg":
            if s.size() > 0:
                x = str(s.pop())
                if inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x):
                        s.push(int(x) * -1)
                    else:
                        s.push(tempX)
                        s.push(":error:")
                elif isNum(x):
                    s.push(int(x) * -1)
                else:
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "swap":
            if s.size() > 1:
                x = s.pop()
                y = s.pop()
                s.push(x)
                s.push(y)
            else:
                s.push(":error:")

        elif command == ":error:":
            s.push(":error:")

        elif command == "and":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isbool(x) and isbool(y):
                        if x == ":true:" and y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isbool(x) and isbool(y):
                        if x == ":true:" and y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isbool(x) and isbool(y):
                        if x == ":true:" and y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isbool(x) and not inDic(dic, y) and isbool(y):
                    if x == ":true:" and y == ":true:":
                        s.push(":true:")
                    else:
                        s.push(":false:")
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "or":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isbool(x) and isbool(y):
                        if x == ":true:" or y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isbool(x) and isbool(y):
                        if x == ":true:" or y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isbool(x) and isbool(y):
                        if x == ":true:" or y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isbool(x) and not inDic(dic, y) and isbool(y):
                    if x == ":true:" or y == ":true:":
                        s.push(":true:")
                    else:
                        s.push(":false:")
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "not":
            if s.size() > 0:
                tempX = x
                x = str(s.pop())
                if inDic(dic, x):
                    x = str(dic[x])
                    if isbool(x):
                        if x == ":true:":
                            s.push(":false:")
                        else:
                            s.push(":true:")
                    else:
                        s.push(tempX)
                        s.push(":error:")
                elif isbool(x):
                    if x == ":true:":
                        s.push(":false:")
                    else:
                        s.push(":true:")
                else:
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "equal":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        if x == y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y):
                        if x == y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        if x == y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y):
                    if x == y:
                        s.push(":true:")
                    else:
                        s.push(":false:")
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "lessThan":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        if x > y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y):
                        if x > y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        if x > y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y):
                    if x > y:
                        s.push(":true:")
                    else:
                        s.push(":false:")
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "bind":
            if s.size() > 1:
                x = s.pop()
                y = s.pop()
                if inDic(dic, y) and inDic(dic, x):
                    if dic[x] == None:
                        s.push(":error:")
                    else:
                        dic[y] = dic[x]
                        s.push(":unit:")
                        if i == len(input) - 1:
                            ans.append(":unit:")
                            return ans
                elif inDic(dic, y) and not x == ":error:":
                    dic[y] = x
                    s.push(":unit:")
                    if i == len(input) - 1:
                        ans.append(":unit:")
                        return ans

                else:
                    s.push(":error:")

            else:
                s.push(":error:")

        elif command == "if":
            if s.size() > 2:
                x = str(s.pop())
                y = str(s.pop())
                z = str(s.pop())
                if inDic(dic, z):
                    tempZ = z
                    z = str(dic[z])
                    if isbool(z):
                        if z == ":true:":
                            s.push(x)
                        else:
                            s.push(y)
                    else:
                        s.push(tempZ)
                        s.push(y)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, z) and isbool(z):
                    if isbool(z):
                        if z == ":true:":
                            s.push(x)
                        else:
                            s.push(y)
                    else:
                        s.push(z)
                        s.push(y)
                        s.push(x)
                        s.push(":error:")
                else:
                    s.push(z)
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "let":
            letLol = getlet(input)
            tempDic = dic.copy()
            answer = letEnd(letLol[letCount], dic)
            dic = tempDic
            s.push(answer[0])

            i = i + len(letLol[letCount])

            if i + 1 == len(input) - 1:
                if answer[0] == ":unit:":
                    ans.append(":unit:")
                    return ans

            letCount += 1

        elif command == "end":
            s = s

        elif command == "fun":
            funName = lineArray[1]
            arg = lineArray[2]
            funArray = getfun(input[i:])

            dic[funName] = funArray
            tempDic = dic.copy()
            funArray.append(dic)
            dic = tempDic
            funArray.append(arg)

            s.push(":unit:")

            i = i + len(funArray) - 1

        elif command == "funEnd":
            s = s

        elif command == "call":
            if s.size() > 1:
                funct = str(s.pop())
                argu = str(s.pop())
                if inDic(dic, argu) and inDic(dic, funct):
                    if isinstance(dic[funct], list):
                        tempDic = dic.copy()
                        if str(evalfun(dic[funct], dic[argu])[-1:]) == "$":
                            s.push(evalfun(dic[funct], dic[argu])[:-1])
                            value = evalfun(dic[funct], dic[argu])[:-1]
                            tempDic[argu] = value
                        elif str(evalfun(dic[funct], dic[argu])[-1:]) == "#":
                            value = evalfun(dic[funct], dic[argu])[:-1]
                            tempDic[argu] = value
                        else:
                            s.push(evalfun(dic[funct], dic[argu]))

                        dic = tempDic
                    else:
                        s.push(argu)
                        s.push(funct)
                        s.push(":error:")
                elif inDic(dic, funct) and argu == ":error:":
                    s.push(argu)
                    s.push(funct)
                    s.push(":error:")
                elif inDic(dic, funct) and not inDic(dic, argu):
                    if isinstance(dic[funct], list):
                        tempDic = dic.copy()
                        if str(evalfun(dic[funct], argu)[-1:]) == "$":
                            s.push(evalfun(dic[funct], argu)[:-1])
                            value = evalfun(dic[funct], argu)[:-1]

                            tempDic[argu] = value
                        elif str(evalfun(dic[funct], argu)[-1:]) == "#":
                            value = evalfun(dic[funct], argu)[:-1]
                            dic[argu] = value
                        else:
                            s.push(evalfun(dic[funct], argu))
                        dic = tempDic
                    else:
                        s.push(argu)
                        s.push(funct)
                        s.push(":error:")
                else:
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "inOutFun":
            funName = lineArray[1]
            arg = lineArray[2]
            funArray = getfun(input[i:])

            dic[funName] = funArray
            tempDic = dic.copy()
            funArray.append(dic)
            dic = tempDic
            funArray.append(arg)
            funArray.append("sai")

            s.push(":unit:")

            i = i + len(funArray) - 2


        else:
            s.push(":error:")

        i += 1

        # x = s.printStack()
        # print (x)
        # print (dic)
    # for a in x:
    #     print (a)
    # print('\n')
    ans.append(s.pop())

    return ans

def interpreter(input, output):
    fIn = open(input, 'r')
    fOut = open(output, 'w')

    inputList = []
    for l in fIn.readlines():
        l = l.strip()
        inputList.append(l)

    s = Stack()
    dic = {}
    letCount = 0
    i = 0
    while i < len(inputList):
        line = inputList[i]

        lineArray = line.split()
        command = lineArray[0]

        if len(lineArray) > 2 and isPushString(line):
            line = line[5:]
            value = line
        elif len(lineArray) == 3 and (command == "fun" or command == "inOutFun"):
            funName = lineArray[1]
            arg = lineArray[2]
        elif len(lineArray) == 2:
            value = lineArray[1]
        elif len(lineArray) == 1:
            command = command

        if command == "quit":
            break

        elif command == "push":

            if value == "-0":
                s.push(0)

            elif inDic(dic, value):
                s.push(value)

            elif isString(value): # check if it is a string
                if value[1:len(value)-1].isalnum:
                    s.push(value[1:len(value)-1])
                else:
                    s.push(":error:")

            elif value[0] == '\"': # check if it is a string
                # if value.strip("\"").isalnum:
                #     s.push(value.strip("\""))
                # else:
                #     s.push(":error:")
                s.push(value)

            elif isNum(value):
                s.push(value)

            elif value.isalnum(): # check if it is a name
                s.push(value)
                dic.update({value : None})

            elif isfloat(value):
                s.push(":error:")

            else:
                s.push(":error:")

        elif command == "pop":
            if s.is_empty():
                s.push(":error:")
            else:
                s.pop()

        elif command == ":true:":
            s.push(":true:")

        elif command == ":false:":
            s.push(":false:")

        elif command == "sub":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())

                if inDic(dic, y) and inDic(dic, x):
                    tempY = y
                    tempX = x
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y - x))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y - x))
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y - x))
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y):
                    x = int(x)
                    y = int(y)
                    s.push(str(y - x))
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "add":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y + x))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y + x))
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(y + x))
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y):
                    x = int(x)
                    y = int(y)
                    s.push(str(y + x))
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "mul":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y * x)))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y * x)))
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y * x)))
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y):
                    x = int(x)
                    y = int(y)
                    s.push(str(int(y * x)))
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "div":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())

                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y / x)))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y / x)))
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y / x)))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y) and not x == "0":
                    x = int(x)
                    y = int(y)
                    s.push(str(int(y / x)))
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "rem":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())

                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y % x)))
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y % x)))
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y) and not x == "0":
                        x = int(x)
                        y = int(y)
                        s.push(str(int(y % x)))
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y) and not x == "0":
                    x = int(x)
                    y = int(y)
                    s.push(str(int(y % x)))
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "neg":
            if s.size() > 0:
                x = str(s.pop())
                if inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x):
                        s.push(str(int(x) * -1))
                    else:
                        s.push(tempX)
                        s.push(":error:")
                elif isNum(x):
                    s.push(str(int(x) * -1))
                else:
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "swap":
            if s.size() > 1:
                x = s.pop()
                y = s.pop()
                s.push(x)
                s.push(y)
            else:
                s.push(":error:")

        elif command == ":error:":
            s.push(":error:")

        elif command == "and":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isbool(x) and isbool(y):
                        if x == ":true:" and y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isbool(x) and isbool(y):
                        if x == ":true:" and y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isbool(x) and isbool(y):
                        if x == ":true:" and y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isbool(x) and not inDic(dic, y) and isbool(y):
                    if x == ":true:" and y == ":true:":
                        s.push(":true:")
                    else:
                        s.push(":false:")
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "or":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isbool(x) and isbool(y):
                        if x == ":true:" or y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isbool(x) and isbool(y):
                        if x == ":true:" or y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isbool(x) and isbool(y):
                        if x == ":true:" or y == ":true:":
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isbool(x) and not inDic(dic, y) and isbool(y):
                    if x == ":true:" or y == ":true:":
                        s.push(":true:")
                    else:
                        s.push(":false:")
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "not":
            if s.size() > 0:
                tempX = x
                x = str(s.pop())
                if inDic(dic, x):
                    x = str(dic[x])
                    if isbool(x):
                        if x == ":true:":
                            s.push(":false:")
                        else:
                            s.push(":true:")
                    else:
                        s.push(tempX)
                        s.push(":error:")
                elif isbool(x):
                    if x == ":true:":
                        s.push(":false:")
                    else:
                        s.push(":true:")
                else:
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "equal":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        if x == y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y):
                        if x == y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        if x == y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y):
                    if x == y:
                        s.push(":true:")
                    else:
                        s.push(":false:")
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "lessThan":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    tempY = y
                    x = str(dic[x])
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        if x > y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(tempX)
                        s.push(":error:")
                elif not inDic(dic, y) and inDic(dic, x):
                    tempX = x
                    x = str(dic[x])
                    if isNum(x) and isNum(y):
                        if x > y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(y)
                        s.push(tempX)
                        s.push(":error:")
                elif inDic(dic, y) and not inDic(dic, x):
                    tempY = y
                    y = str(dic[y])
                    if isNum(x) and isNum(y):
                        if x > y:
                            s.push(":true:")
                        else:
                            s.push(":false:")
                    else:
                        s.push(tempY)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, x) and isNum(x) and not inDic(dic, y) and isNum(y):
                    if x > y:
                        s.push(":true:")
                    else:
                        s.push(":false:")
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "bind":
            if s.size() > 1:
                x = str(s.pop())
                y = str(s.pop())
                if inDic(dic, y) and inDic(dic, x):
                    if dic[x] == None:
                        s.push(y)
                        s.push(x)
                        s.push(":error:")
                    else:
                        dic[y] = dic[x]
                        s.push(":unit:")
                elif inDic(dic, y) and not x == ":error:":
                    dic[y] = x
                    s.push(":unit:")
                else:
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "if":
            if s.size() > 2:
                x = str(s.pop())
                y = str(s.pop())
                z = str(s.pop())
                if inDic(dic, z):
                    tempZ = z
                    z = str(dic[z])
                    if isbool(z):
                        if z == ":true:":
                            s.push(x)
                        else:
                            s.push(y)
                    else:
                        s.push(tempZ)
                        s.push(y)
                        s.push(x)
                        s.push(":error:")
                elif not inDic(dic, z) and isbool(z):
                    if isbool(z):
                        if z == ":true:":
                            s.push(x)
                        else:
                            s.push(y)
                    else:
                        s.push(z)
                        s.push(y)
                        s.push(x)
                        s.push(":error:")
                else:
                    s.push(z)
                    s.push(y)
                    s.push(x)
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "let":

            letLol = getlet(inputList)
            letLol[0].append("end")
            letLol[0].insert(0, "let")
            tempDic = dic.copy()
            ans = letEnd(letLol[letCount], dic)
            dic = tempDic
            s.push(ans[0])

            i = i + len(letLol[letCount])-1

            letCount += 1

        elif command == "end":
            s = s

        elif command == "fun":
            funName = lineArray[1]
            arg = lineArray[2]
            funArray = getfun(inputList[i:])

            dic[funName] = funArray
            tempDic = dic.copy()
            funArray.append(dic)
            dic = tempDic
            funArray.append(arg)

            s.push(":unit:")

            i = i + len(funArray) - 1

        elif command == "funEnd":
            s = s

        elif command == "call":
            if s.size() > 1:
                funct = str(s.pop())
                argu = str(s.pop())
                if inDic(dic, argu) and inDic(dic, funct):
                    if isinstance(dic[funct], list):
                        tempDic = dic.copy()
                        if str(evalfun(dic[funct], dic[argu])[-1:]) == "$":
                            s.push(evalfun(dic[funct], dic[argu])[:-1])
                            value = evalfun(dic[funct], dic[argu])[:-1]
                            tempDic[argu] = value
                        elif str(evalfun(dic[funct], dic[argu])[-1:]) == "#":
                            value = evalfun(dic[funct], dic[argu])[:-1]
                            tempDic[argu] = value
                        else:
                            s.push(evalfun(dic[funct], dic[argu]))

                        dic = tempDic
                    else:
                        s.push(argu)
                        s.push(funct)
                        s.push(":error:")
                elif inDic(dic, funct) and argu == ":error:":
                    s.push(argu)
                    s.push(funct)
                    s.push(":error:")
                elif inDic(dic, funct) and not inDic(dic, argu):
                    if isinstance(dic[funct], list):
                        tempDic = dic.copy()
                        if str(evalfun(dic[funct], argu)[-1:]) == "$":
                            s.push(evalfun(dic[funct], argu)[:-1])
                            value = evalfun(dic[funct], argu)[:-1]

                            tempDic[argu] = value
                        elif str(evalfun(dic[funct], argu)[-1:]) == "#":
                            value = evalfun(dic[funct], argu)[:-1]
                            dic[argu] = value
                        else:
                            s.push(evalfun(dic[funct], argu))
                        dic = tempDic
                    else:
                        s.push(argu)
                        s.push(funct)
                        s.push(":error:")
                else:
                    s.push(":error:")
            else:
                s.push(":error:")

        elif command == "inOutFun":
            funName = lineArray[1]
            arg = lineArray[2]
            funArray = getfun(inputList[i:])

            dic[funName] = funArray
            tempDic = dic.copy()
            funArray.append(dic)
            dic = tempDic
            funArray.append(arg)
            funArray.append("sai")

            s.push(":unit:")

            i = i + len(funArray) - 2

        else:
            s.push(":error:")

        i += 1

    x = s.printStack()
    x = removeQuotes(x)

    # print (x)
    # print (dic)

    for ans in x:
        ans = str(ans)
        fOut.write(ans + '\n')

    fIn.close()
