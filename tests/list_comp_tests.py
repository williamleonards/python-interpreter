# Standard list comprehension
x = [i for i in range(5)]
assert(x == [0, 1, 2, 3, 4])

# 2-d matrix
x = [[i for i in range(5)] for j in range(6) if j % 2 == 0]
assert(x == [[0,1,2,3,4], [0,1,2,3,4], [0,1,2,3,4]])

# Filter out none values
x = [None, 1, 2, None, 5, ""]
filtered_x = [i for i in x if i]
assert(filtered_x == [1, 2, 5])