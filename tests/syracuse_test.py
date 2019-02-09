# syracuse series of Collatz conjecture
# Test early return, if/else inside a function
def syra(x, iter):
    if x == 1:
        return iter
    if x % 2 == 0:
        return syra(x/2, iter+1)
    else:
        return syra(3 * x + 1, iter+1)

assert(syra(27, 0) == 111)
assert(syra(9, 0) == 19)
assert(syra(97, 0) == 118)
assert(syra(871, 0) == 178)