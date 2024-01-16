def concatenated(xs: list):
    y = [] 
    for x in xs:
        y.extend(x)
    return y
##########################################

def mixable(xs:list):
    options = []
    for recipe in recipes:
        for ingredient in recipe[1]:
            if not ingredient in xs:
                break 
        else:
            options=options +[recipe[0]]
    return options  



    
recipes =  [("Daiquiri", ["Rum", "Limette", "Zucker"]),
            ("Mojito", ["Rum", "Limette", "Zucker", "Minze", "Soda"]), 
            ("Whiskey Sour", ["Whiskey", "Zitrone", "Zucker"]), ("Tequila Sour", 
            ["Tequila", "Zitrone", "Zucker"]), ("Moscow Mule", ["Vodka", "Limette", "Ginger ale"]), 
            ("Munich Mule", ["Gin", "Limette", "Ginger ale"]),
            "Cuba Libre", ["Rum", "Coke"]]
################################################################
def to_binary(x , n ):
    b = []
    i = 0
    while i < n:
        res = x%2 
        x = x//2
        b = [res]+b 
        i +=1 
    return b

def primes(n: int) -> bool:
    one = 1
    amazonprime = []
    if n%2 == 1 and n//1==n:
        return True 
    else: 
        return False 

def plist(n:int)-> list:
    l = []
    for i in range(0,n):
        if primes(i) == True: 
            l.append(i)
        else: 
            continue
    return l
    
