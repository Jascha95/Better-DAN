######################################################################
## visible code

import math
import random
import string

######################################################################
## student code

def fib(n: int) -> int:
    """Returns the `n`-th fibonacci number."""
    fibs = [0, 1]
    for i in range(2, n + 1):
        fibs += [fibs[i-1] + fibs[i-2]]
    return fibs[n]

