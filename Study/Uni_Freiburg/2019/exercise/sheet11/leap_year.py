def leap_year(year: int) -> bool:
    """Returns whether `year` is a leap year."""
    if year % 4 != 0:
        return False
    elif year % 400 == 0:
        return True
    elif year % 100 == 0:
        return False
    else:
        return True
