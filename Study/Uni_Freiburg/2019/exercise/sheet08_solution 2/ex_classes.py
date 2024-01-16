######################################################################
## visible code

import math
import random
import string

######################################################################
## student code

class Ingredient:
    def __init__(self, name: str, price: int, veggi: bool):
        '''Creates an `Ingredient` given its `name`, its `price` in cents,
        and whether it's `veggi`.
        '''
        self.name = name
        self.price = price
        self.veggi = veggi

class Recipe:
    def __init__(self, name: str, ingredients: list):
        '''Creates a `Recipe` given its `name` and a list of its `ingredients`.'''
        self.name = name
        self.ingredients = ingredients

def recipe_price(recipe: Recipe) -> int:
    '''Takes a `recipe` and returns it's total price in cents.'''
    price = 0
    for i in recipe.ingredients:
        price = price + i.price
    return price

def is_veggi(recipe: Recipe) -> bool:
    '''Takes a `recipe` and returns whether its veggi.'''
    veggi = True
    for i in recipe.ingredients:
        if not i.veggi:
            veggi = False
    return veggi

def ingredient_str(ingredient: Ingredient) -> str:
    '''Takes an `ingredient` and returns its name.'''
    return ingredient.name

def recipe_str(recipe: Recipe) -> str:
    '''Takes a `recipe` and returns a human readable string representation.'''
    string = "A " + recipe.name + " has "
    for i in recipe.ingredients[0:-2]:
        string = string + ingredient_str(i) + ", "
    if len(recipe.ingredients) >= 2:
        string = string + ingredient_str(recipe.ingredients[-2]) + " and "
    if len(recipe.ingredients) >= 1:
        string = string + ingredient_str(recipe.ingredients[-1]) + "."
    else:
        string = string + "nothing."
    if is_veggi(recipe):
        string = string + " It is vegetarian"
    else:
        string = string + " It is not vegetarian"
    string = string + " and costs " + repr(recipe_price(recipe)/100.) + " Euro."
    return string

