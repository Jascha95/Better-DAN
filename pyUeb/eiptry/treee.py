def next_word(s:str):
    count = 0
    if not s:
        return ""
    for letter in s:
        if letter.isalpha():
            count += count
        else: 
            break 
        return (s[:count], s[count:])
          