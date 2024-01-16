######################################################################
## visible code

import math
import random
import string

######################################################################
## student code

def search(key, wb):
    '''Searches for a `key` in a dictionary `wb`, represented as a list of
    key-value-pairs.'''
    for kv in wb:
        if kv[0] == key:
            return kv[1]
    return None

def remove(key, wb):
    '''Removes a `key` from a dictionary `wb`, represented as a list of
    key-value-pairs, and returns the new dictionary.
    The old dictionary is left unchanged.

    If the `key` is not present, the dictionary is returned unmodified.'''
    wb2 = []
    for kv in wb:
        if kv[0] != key:
            wb2 += [kv]
    return wb2

def insert(key, value, wb):
    '''Inserts a `value` at a `key` into a dictionary `wb`, represented as a list of
    key-value-pairs, and returns the new dictionary.
    The old dictionary is left unchanged.

    If the `key` is already present, the old entry is removed first.'''
    wb2 = remove(key, wb)
    return wb2 + [(key, value)]

