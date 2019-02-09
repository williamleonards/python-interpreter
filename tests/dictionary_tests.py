a = {}
assert(not a)

a[1] = 5
a[2] = 10

assert(a[1] == 5)
assert(a[2] == 10)

assert(a.get(1) == 5)
assert(a.get(2) == 10)

a.put(3, 5)
assert(a.get(3) == 5)