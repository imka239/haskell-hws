fr = open("a.in", "r")
fw = open("a.out", "w")
describe = ""
while(True):
    s = fr.readline().strip()
    if (s == "0"):
        break
    if ("::" in s or s == ""):
        continue
    now = 0
    while(s[now] != '\"'):
        now += 1
    x = s[now:len(s)]
    kek = x.split("~:")
    lol = kek[2].split("~=?")
    kekw = kek[0].split('.')
    kekw = kekw[1]
    desc = ""
    ind = 0
    while (kekw[ind] < '0' or kekw[ind] > '9'):
        desc += kekw[ind]
        ind += 1
    if (desc != describe):
        print("  describe \"", desc, "\" $ do", file = fw, sep="")
        describe = desc

    print("    it", kek[1], "$", file = fw, sep="")
    print("     ", lol[0], " `shouldBe`", lol[1][0:len(lol[1]) - 1], file = fw, sep="")
    print(x)