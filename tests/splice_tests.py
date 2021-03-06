a = [0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

assert(a[-1] == 10)
assert(a[1] == 1)
assert(a[5] == 5)

assert( a[1:] == [1,2,3,4,5,6,7,8,9,10])
assert( a[9:] == [9,10])

assert(a[::-1] == [10,9,8,7,6,5,4,3,2,1,0])

assert(a[::-2] == [10, 8, 6, 4, 2, 0])
assert(a[4:8:3] == [4,7])

assert(a[:3] == [0, 1, 2])
assert(a[:3:] == [0,1,2])
assert(a[0:3:] == [0,1,2])
assert(a[4:3:] == [])
assert(a[4:3:1] == [])

assert(a[4:8:-1] == [])