# !/usr/bin/python
# -*- coding: utf-8 -*-


def languageIdentification(word):

    if word.endswith(('heit','keit','chen')):
        language = 'German'
    elif word.endswith(('ness','nce','ty')):
        language = 'English'
    elif word.endswith(('que','age')):
        language = 'French'
    else:
        language = 'The language of the word cannot be determined.'
    return language

# Input of a noun:
word = input('Enter a noun in German, English or French: ')

print(languageIdentification(word))