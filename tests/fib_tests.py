# fibonacci function
def fib(x):
    if x == 0 or x == 1:
        return 1
    return fib(x-2) + fib(x-1)

# Check fibonacci numbers
assert(fib(8) == 34)
assert(fib(9) == 55)
assert(fib(10) == 89)

# Reference one function from another
def fib5(x):
    return fib(5)

# Check values are correct
assert(fib5(0) == fib5(1))
assert(fib5(0) == 8)