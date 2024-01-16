s = input("Dezimalzahl: ")
n = int(s)
b0 = (n // 2 ** 0)%2
b1 = (n // 2 ** 1)%2
b2 = (n // 2 ** 2)%2
b3 = (n // 2 ** 3)%2
b4 = (n // 2 ** 4)%2
b5 = (n // 2 ** 5)%2
b6 = (n // 2 ** 6)%2
b7 = (n // 2 ** 7)%2
print("Binärdarstellung:", b7, b6, b5, b4, b3, b2, b1, b0)
