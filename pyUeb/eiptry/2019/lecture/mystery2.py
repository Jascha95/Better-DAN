from lsystem import *
from turtle import *
import random

l_ct = LSystem ( 
    "LLLLLLSLFFFX",
    {
        'S': "[+++G][---g]TS",
        'G': "+H[-G]CL",
        'H': "-G[+H]CL",
        'g': "+h[-g]cL",
        'h': "-g[+h]cL",
        'T': "TL",
        'L': "[-FFF][+FFF]F"
    })


def run (ls : LSystem, n : int, s : int, a : int, r=0):
    w = ls.generate(n)
    pointsleft = []
    pointsright = []
    positions = []
    angles = []
    for c in w:
        if c == 'F' or c == 'K':
            pendown()
            forward(s)
        elif c == 'f':
            penup()
            forward(s)
        elif c == '[':
            positions.append (position())
            angles.append (heading())
        elif c == ']':
            setposition (positions.pop())
            setheading (angles.pop())
        elif c == '+':
            left(a * (1 + r * (random.random() - 0.5)))
        elif c == '-':
            right(a * (1 + r * (random.random() - 0.5)))
        elif c == 'c':
            pointsleft.append (position())
        elif c == 'C':
            pointsright.append (position())
        elif c == 'X':
            right(15)
            color('red','yellow')
            begin_fill()
            for i in range(12):
                forward(3*s)
                left(150)
            end_fill()
            color('green')
        else:
            pass
    color ('black')
    for points in (pointsleft, pointsright):
        if points:
            for i in range(15):
                penup()
                setposition (random.choice (points))
                setheading(90)
                pendown()
                forward(-2*s)
                dot(s,random.choice (["red", "blue", "yellow"]))

def run_ct(n):
    reset()
    left(90)
    penup()
    forward(-300)
    color('green')
    speed(0)
    hideturtle()
    run (l_ct, n, s=20, a=16, r=0.2)
