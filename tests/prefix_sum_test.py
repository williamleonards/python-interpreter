def prefix_arr(arr):
    sums = [arr[0]]
    i = 1
    while i < len(arr):
        sums.append(sums[-1] + arr[i])
        i = i + 1
    return sums

prefix_sums = prefix_arr([1,2,3,4,5,6,7])
assert(prefix_sums == [1, 3, 6, 10, 15, 21, 28])

prefix_sums = prefix_arr([12,10,5,8,9])
prefix_sums = prefix_arr([12,22,27,35,44])