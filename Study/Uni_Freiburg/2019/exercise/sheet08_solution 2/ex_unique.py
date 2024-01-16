######################################################################
## visible code

import math
import random
import string

######################################################################
## student code

def unique(xs: list) -> list:
    '''Takes a sorted list of integers `xs` and returns a list that contains
    all elements of `xs`, but without duplicates.
    '''
    prev_x = None
    ys = []
    for x in xs:
        if prev_x != x:
            prev_x = x
            ys += [x]
    return ys

