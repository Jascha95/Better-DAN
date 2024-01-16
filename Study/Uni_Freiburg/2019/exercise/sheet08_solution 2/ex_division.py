######################################################################
## visible code

import math
import random
import string

######################################################################
## student code

def divide(a: int, b: int) -> int:
    '''Returns the integer division of `a` by `b`.'''
    x = 0
    while a >= b:
        a = a - b
        x = x + 1
    return x

