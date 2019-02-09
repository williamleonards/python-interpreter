# from https://github.com/TheAlgorithms/Python/blob/master/dynamic_programming/integer_partition.py
# The number of partitions of a number n into at least k parts equals the number of partitions into exactly k parts
# plus the number of partitions into at least k-1 parts. Subtracting 1 from each part of a partition of n into k parts
# gives a partition of n-k into k parts. These two facts together are used for this algorithm.


def partition(m):
    memo = [[0 for _ in range(m)] for _ in range(m+1)]
    for i in range(m+1):
        memo[i][0] = 1

    for n in range(m+1):
        for k in range(1, m):
            memo[n][k] += memo[n][k-1]
            if n-k > 0:
                memo[n][k] += memo[n-(k+1)][k]

    return memo[m][m-1]


assert(partition(10) == 42)
assert(partition(30) == 5604)
assert(partition(50) == 204226)
