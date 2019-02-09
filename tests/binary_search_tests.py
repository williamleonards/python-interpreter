# Return the value right of val
def bin_search_right(arr, val):
    lo = 0 
    hi = len(arr) - 1
    while lo < hi:
        mid = (lo + hi)//2
        if arr[mid] <= val:
            lo = mid + 1
        else:
            hi = mid
    return lo

# Return the value left of val
def bin_search_left(arr, val):
    lo = 0 
    hi = len(arr) - 1
    while lo < hi:
        mid = (lo + hi + 1)//2
        if arr[mid] < val:
            lo = mid 
        else:
            hi = mid - 1
    return lo

arr = [1, 3, 5, 7, 7, 7, 9, 11, 11, 11, 13, 16, 22, 24, 25, 25, 26, 27, 27, 29]
assert(bin_search_left(arr, 7) == 2)
assert(bin_search_right(arr, 7) == 6)
assert(bin_search_left(arr, 11) == 6)
assert(bin_search_right(arr, 11) == 10)