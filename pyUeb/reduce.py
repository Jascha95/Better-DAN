

def my_reduce(f, xs, x):
    f = f
    xs = []
    x =  x
    assert x>0,"huston we have a problem"
    return list((lambda f, xs: f * xs, range(x)))
    
#>>> reduce(lambda x, y: x * y, range(1, 5))
#24 # ((1 * 2) * 3) * 4
#>>> def product(it):
#... return reduce (lambda x,y: x*y, it, 1)





