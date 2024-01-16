def fahrenheit_to_celsius(fahrenheit):
    return (fahrenheit - 32) * (5 / 9)

def celsius_to_kelvin(celsius):
    return celsius+ 273.15

def fahrenheit_to_kelvin(fahrenheit):
    return celsius_to_kelvin(fahrenheit_to_celsius()) 

fahrenheit = float(input("Fahrenheit: "))

celsius = fahrenheit_to_celsius(fahrenheit)
kelvin = fahrenheit_to_celsius(fahrenheit)

print("Celsius:", round(celsius, 2))
print("Kelvin:", round(kelvin, 2))