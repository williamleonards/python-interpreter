# taken from https://leetcode.com/problems/longest-increasing-subsequence/discuss/74824/JavaPython-Binary-search-O(nlogn)-time-with-explanation
def lengthOfLIS(nums):
    tails = [0] * len(nums)
    size = 0
    for x in nums:
        i = 0
        j = size
        while i != j:
            m = (i + j) // 2
            if tails[m] < x:
                i = m + 1
            else:
                j = m
        tails[i] = x
        size = max(i + 1, size)
    return size

arr = [3, 30, 40, 28, 41, 8, 25, 22, 30, 26, 48, 15, 39, 24, 34, 38, 4, 34, 4, 11, 9, 20, 32, 6, 15, 37, 50, 19, 12, 29]
assert(lengthOfLIS(arr) == 7)
assert(lengthOfLIS([10,9,2,5,3,7,101,18]) == 4)