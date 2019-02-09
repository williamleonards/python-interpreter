def merge(arr): 
    if len(arr) > 1: 
        mid = len(arr)//2 
        L = arr[:mid] 
        R = arr[mid:]
  
        merge(L)
        merge(R)
  
        i = 0
        j = 0
        k = 0
          
        while i < len(L) and j < len(R): 
            if L[i] < R[j]: 
                arr[k] = L[i] 
                i+=1
            else: 
                arr[k] = R[j] 
                j+=1
            k+=1
          
        while i < len(L): 
            arr[k] = L[i] 
            i+=1
            k+=1
          
        while j < len(R): 
            arr[k] = R[j] 
            j+=1
            k+=1
  
arr = [12, 11, 13, 5, 6, 7]  
merge(arr)
assert(arr == [5,6,7,11,12,13])

large_input = [3, 30, 40, 28, 41, 8, 25, 22, 30, 26, 48, 15, 39, 24, 34, 38, 4, 34, 4, 11, 9, 20, 32, 6, 15, 37, 50, 19, 12, 29]

sorted_input = [3, 4, 4, 6, 8, 9, 11, 12, 15, 15, 19, 20, 22, 24, 25, 26, 28, 29, 30, 30, 32, 34, 34, 37, 38, 39, 40, 41, 48, 50]

merge(large_input)
assert(large_input == sorted_input)