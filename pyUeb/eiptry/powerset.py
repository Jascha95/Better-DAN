def power(x,n,acc=1):
    while n != 0:
        n, acc = n-1,acc*x
    else: 
        return acc

def fast_power (x,n):
    if n == 0:
        return 1
    elif n%2 == 0:
        return fast_power(x*x,n//2)
    else return x *fast_power(x*x,n//2)