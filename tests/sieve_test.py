def sieve(n):
    l = [True] * (n+1)
    prime = []
    start = 2
    end   = len(l) - 1
    while(start <= end):
        if l[start] == True:
            prime.append(start)
            for i in range(start*start, n+1, start):
                if l[i] == True:
                    l[i] = False
        start += 1
    
    for j in range(end+1,n+1):
        if l[j] == True:
            prime.append(j)
    
    return prime

primes = [2, 3, 5, 7, 11, 13, 17, 19, 23, 29, 31, 37, 41, 43, 47, 53, 59, 61, 67, 71, 73, 79, 83, 89, 97]
assert(sieve(100) == primes)