#f_mappped = map(ctof,c_list )

#f_lambda = map(lambda c:1.8*c+32, c_list)

c_list = [5,6,26]

def gen_ctof (cl):
    for c in cl: 
        yield ctof(c)
f_gen = gen_ctof (c_list)
