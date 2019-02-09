# Test basic while loop
x = 0
y = 0
count = 20
while count > 0:
    x = x + 1
    y = y + 1
    assert(x == y)
    count = count - 1
    assert(count + x == 20)

assert(x == 20)
assert(y == 20)

# If inside while loop 
x = 0
while x < 20:
    if x:
        assert(x > 0)
    x += 1

# Nested while loop
x = 0
count = 0
while x < 20:
    count += 1
    while x < 10:
        x += 1
    x += 1
assert(x == 20)
assert(count == 10)