from functools import lru_cache

#@lru_cache(maxsize = 10000)
def fibu(n):
    if n == 1:
        return 1 
    elif n == 2:
        return 1
    elif n > 2:
        return fibu(n-1) + fibu(n-2)
ns = input("wie viele fibu's willste sehn':")
s = int(ns)
for n in range(1,s):
    print(n ,":", fibu(n))