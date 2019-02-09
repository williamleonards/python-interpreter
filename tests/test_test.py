def isPrime(n):
    if n < 2:
        return (False)
    for i in range(1, (n//2)+1):
        if n % i == 0 and (i >= 2):
            return (False)
    return (True)

assert(isPrime(13))
assert(not isPrime(12))
assert(not isPrime(25))
assert(not isPrime(30))
assert(not isPrime(39))
assert(not isPrime(57))