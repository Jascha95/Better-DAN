def queens(n: int):
    """Returns a generator of solutions for the `n`-queens problem.
A solution is modeled as a `n`-tuple of integers, where the `i`-th entry of the tuple describes on which row the queen at column `i` should be placed. """
    return queens_recursive(tuple(), n)


def queens_recursive(t: tuple, n: int):
    if len(t) == n:
        yield t 
    else:
        for row in range(n): 
            t1 = t + (row,)
    if new_queen_is_valid(t1):
        yield from queens_recursive(t1, n)

def new_queen_is_valid(t: tuple) -> bool:
    last_col = len(t)-1
    for col in range(last_col):
        if t[last_col] == t[col]: # horizontal conflict 
            return False
        if t[last_col] == t[col] + (last_col - col): # diagonal conflict 1 
            return False
        if t[last_col] == t[col] - (last_col - col): # diagonal conflict 2 
            return False
    return True