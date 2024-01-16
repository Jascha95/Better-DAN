######################################################################
## visible code

import math
import random
import string

######################################################################
## student code

def find_index(start: int, c: str, s: str):
    for i in range(start, len(s)):
        if s[i] == c:
            return i

def keywords(c: str, s: str) -> list:
    '''Find all keywords in string `s` between two occurences of `c`.'''
    i = 0
    keywords = []
    while i < len(s):
        start = find_index(i, c, s)
        if start == None:
            break
        end = find_index(start + 1, c, s)
        if end == None:
            break
        keywords += [ s[start+1 : end] ]
        i = end + 1
    return keywords

