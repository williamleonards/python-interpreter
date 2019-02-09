# Asserting works the same as boolean casting

# Positive cases
assert(1)
assert("abc")
assert([1])
# TODO: this fails
# assert({"a": "b"})
# assert({})
assert(True)
assert(1.3)
assert([1, 2])

def f(x): return x*2
assert(f)

# Negative cases 
assert(not 0)
assert(not "")
assert(not [])
# assert(not {})
assert(not 0.0)
assert(not None)