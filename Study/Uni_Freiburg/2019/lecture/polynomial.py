""" A polynomial is
    represented by a list of coefficients in order of ascending exponent
    either the list is empty or the leading coefficient is not zero
"""

import math

def integral(
    p : list # of float (polynom)
    ) -> list:
    """compute the integral of polynomial p"""
    # special case: null polynom
    if len(p) == 0:
        return []
    # initialize accumulator
    result = [0]
    for i, ai in enumerate(p):
        # action for each coeffient
        result = result + [ai / (i+1)]
    return result

print( integral([])  == [] )
print( integral([1]) == [0, 1] )
print( integral([1, 2, 6, 24]) == [ 0, 1, 1, 2, 6 ] )

def derivative(
    p : list # of float
    ) -> list:
    """compute the derivative of polynomial p"""
    result = []
    for i, ai in enumerate(p):
        if i > 0:
            result = result + [ai * i]
    return result

print( derivative([]) == [] )
print( derivative([1]) == [] )
print( derivative([0, 1, 1, 2, 6, 24]) == [1, 2, 6, 24, 120] )

expx = [1, 1, 1/2, 1/6, 1/24, 1/120, 1/720]

def poly_eval( p : list, x0 : float) -> float:
    result = 0
    for i, pi in enumerate(p):
        result = result + pi * x0 ** i
    return result

def newton( f : list, x0 : float ) -> float:
    """ implements newton's zero findining algorithm """
    deriv_f = derivative(f)
    xn = x0
    while not math.isclose( poly_eval( f, xn ), 0, rel_tol=1e-7):
        xn = xn - poly_eval( f, xn ) / poly_eval(deriv_f, xn)
        print("xn =", xn)
    return xn
