from turtle import *

def setup(fg, bg):
    reset()
    speed(0)
    color(fg, bg)
    hideturtle()

def equi_triangle(size):
    forward(size)
    left(120)
    forward(size)
    left(120)
    forward(size)
    left(120)

def koch (size, n):
    if n == 0:
        forward(size)
    else:
        koch (size/3, n-1)
        left(60)
        koch (size/3, n-1)
        right(120)
        koch (size/3, n-1)
        left(60)
        koch (size/3, n-1)

def koch_flake (size, n):
    koch (size, n)
    right (120)
    koch (size, n)
    right (120)
    koch (size, n)
    right (120)

# Fractal (binary) tree
# variables : 0, 1
# constants: [, ]
# axiom : 0
# rules : (1 → 11), (0 → 1[0]0)

# 0 - draw line segment ending in a leaf
def btree_0 (size, n):
    if n == 0:
        forward(size)
        dot (10, 'green')
    else:
        n = n - 1
        btree_1 (size/3, n)
        pos = position()
        ang = heading()
        left(45)
        btree_0 (size/3, n)
        penup()
        setposition (pos)
        setheading (ang)
        pendown()
        right (45)
        btree_0 (size/3, n)

def btree_1 (size, n):
    if n == 0:
        forward (size)
    else:
        n = n - 1
        btree_1 (size/3, n)
        btree_1 (size/3, n)
