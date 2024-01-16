######################################################################
## visible code

import math
import random
import string

######################################################################
## student code

def digits(n: int) -> list:
    '''Takes a natural number `n` and returns the list of its digits in natural
    order.
    '''
    digits = []
    while n > 10:
        digits += [n % 10]
        n = n // 10
    digits += [n]
    digits = digits[::-1]
    return digits

def cross_sum(n: int) -> int:
    '''Takes a natural number `n` and returns the cross sum of `n`'''
    return sum(digits(n))

