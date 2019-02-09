def xorx(x):
    return x*2


def xandx(x): return 3


def xnotx(x): return 3


def xdefx(x): return 3


a = xorx(1)
a = xandx(1)
a = xnotx(1)
a = xdefx(1)


def f(): return 5


assert(f() == 5)
