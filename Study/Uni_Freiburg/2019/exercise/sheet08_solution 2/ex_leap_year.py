######################################################################
## visible code

import math
import random
import string

######################################################################
## student code

def leap(year :int) -> bool:
    """is this year a leap year"""
    if year % 4 != 0:
        return False
    elif year % 400 == 0:
        return True
    elif year % 100 == 0:
        return False
    else:
        return True

