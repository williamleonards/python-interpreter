# https://leetcode.com/problems/maximum-product-subarray/description/
def maxProduct(nums):
    maximum = nums[0]
    big = nums[0]
    small = nums[0]
    for n in nums[1:]:
        n_big = max(n, n*big, n*small)
        n_small = min(n, n*big, n*small)
        big = n_big
        small = n_small
        maximum=max(maximum, big)
    return maximum

arr = [2,3,-2,20,5,14,-2,-3,60,-1,4]
assert(maxProduct(arr) == 24192000)
