def fibugen(n):
    a,b = 0,1
    count = 0
    while count<n:
        c = a+b
        yield c
        a = b
        b = c
        count +=1