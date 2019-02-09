l = []
l.append(5)
assert(l == [5])

l.append(20)
assert(l == [5, 20])

# Index checking
assert(l[0] == 5)
assert(l[1] == 20)

l[1] = 30
assert(l == [5, 30])
assert(l[1] == 30)

l[0] = 10
assert(l == [10, 30])

lc = [i for i in range(10) if i % 2 == 0]
lc = [sora for sora in range(10) if sora % 2 == 0]
