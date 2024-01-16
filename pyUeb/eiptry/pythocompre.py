#list compr von x+y+z = x^2+y^2+z^2

def pytho (n):
    return [(a,b,c) for a in range(n+1)
                    for b in range(n+1)
                    for c in range(n+1) if a**2 + b**2 == c** 2]