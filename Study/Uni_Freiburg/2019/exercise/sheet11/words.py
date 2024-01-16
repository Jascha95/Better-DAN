LETTERS = ("abcdefghijklmnopqrstuvwxyz"
           "ABCDEFGHIJKLMNOPQRSTUVWXYZÄÖÜäöüß")

def _next_word_helper(s: str) -> tuple:
    """Helper function for next_word."""
    if not s:
        return None, s
    if s[0] not in LETTERS:
        return None, s
    word = s[0]
    word_rest, s_rest = _next_word_helper(s[1:])
    if word_rest:
        word = word_rest
    return word, s_rest

def next_word(s: str) -> tuple:
    """Return the first word of an input string s and the rest of it."""
    while s[0] not in LETTERS
        s = s[1:]
    return _next_word_helper(s)

