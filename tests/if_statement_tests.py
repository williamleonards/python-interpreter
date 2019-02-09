if True:
    assert(1)
else:
    assert(0)

if True:
    assert(1)
if True:
    assert(1)
if False:
    assert(0)
else:
    assert(1)

if True:
    assert(1)
elif False:
    assert(0)
elif False:
    assert(0)
elif False:
    assert(0)
else:
    assert(0)

if False:
    assert(0)
elif True:
    assert(1)
elif False:
    assert(0)
elif False:
    assert(0)
else:
    assert(0)

if False:
    assert(0)
elif False:
    assert(0)
elif True:
    assert(1)
elif False:
    assert(0)
else:
    assert(0)