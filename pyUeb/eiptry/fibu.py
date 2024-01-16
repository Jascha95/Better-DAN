
fibu_cache ={}

def fibu(n):
    if n in fibu_cache:
        return fibu_cache[n]
    if n == 1:
        value = 1 
    elif n == 2:
        value = 1 
    elif n > 2:
        value = fibu(n-1) + fibu(n-2)

    fibu_cache[n]=value
    return value 
for n in range(1,1000):
    print(n ,":", fibu(n))