# https://github.com/TheAlgorithms/Python/blob/master/dynamic_programming/coin_change.py
def dp_count(S, m, n):
    table = [0] * (n + 1)

    # Base case (If given value is 0)
    table[0] = 1

    # Pick all coins one by one and update table[] values
    # after the index greater than or equal to the value of the
    # picked coin
    for i in range(0, m):
        for j in range(S[i], n + 1):
            table[j] += table[j - S[i]]
            # @PATRICK, above fails, below works 
            # table[j] = table[j] + table[j - S[i]]

    return table[n]

assert(dp_count([1, 2, 3], 3, 4) == 4) 
assert(dp_count([2, 5, 3, 6], 4, 10) == 5)  