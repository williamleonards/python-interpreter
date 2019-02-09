def partition(arr,low,high):
    i = low-1
    pivot = arr[high]
 
    for j in range(low, high):
        # If current element is smaller than or
        # equal to pivot
        if arr[j] <= pivot:
            i = i + 1
            tmp = arr[j]
            arr[j] = arr[i]
            arr[i] = tmp
 
    tmp = arr[i+1]
    arr[i+1] = arr[high]
    arr[high] = tmp
    return (i+1)

def quickSort(arr,low,high):
    if low < high:
        # pi is partitioning index, arr[p] is now
        # at right place
        pi = partition(arr,low,high)
 
        # Separately sort elements before
        # partition and after partition
        quickSort(arr, low, pi-1)
        quickSort(arr, pi+1, high)

arr = [10, 7, 8, 9, 1, 5]
quickSort(arr,0,len(arr)-1)
assert([1, 5, 7, 8, 9, 10] == arr)


large_input = [3, 30, 40, 28, 41, 8, 25, 22, 30, 26, 48, 15, 39, 24, 34, 38, 4, 34, 4, 11, 9, 20, 32, 6, 15, 37, 50, 19, 12, 29]

sorted_input = [3, 4, 4, 6, 8, 9, 11, 12, 15, 15, 19, 20, 22, 24, 25, 26, 28, 29, 30, 30, 32, 34, 34, 37, 38, 39, 40, 41, 48, 50]

quickSort(large_input,0,len(large_input)-1)
assert(large_input == sorted_input)