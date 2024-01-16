def poly_eval(p: list, x: float) -> float:
    y = 0
    for i,a in enumerate(p):
        y = y +a * x**i
    return y 

def poly_eval_faster(p:list, x:float) -> float:
    y = 0 
    for i in p:
        y = y+x*x*i
    return y