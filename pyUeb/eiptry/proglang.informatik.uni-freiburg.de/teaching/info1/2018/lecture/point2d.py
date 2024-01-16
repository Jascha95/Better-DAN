from math import sqrt, cos, sin, pi, atan2
import numbers

class Point2D:
    def __init__(self, x, y):
        self.x = x
        self.y = y

    def turn(self, phi):
        self.x, self.y = (self.x * cos(phi) - self.y * sin(phi)
                         ,self.x * sin(phi) + self.y * cos(phi))

    def __add__ (self, other):
        if isinstance (other, Point2D):
            return Point2D (self.x + other.x, self.y + other.y)
        else:
            raise TypeError ("Cannot add Point2D and " + str (type (other)))

    def __mul__ (self, other):
        if isinstance (other, Point2D):            # dot product
            return self.x * other.x + self.y * other.y
        elif isinstance (other, numbers.Number):   # scalar multiplication
            return Point2D (other * self.x, other * self.y)
        else:
            raise TypeError ("Cannot multiply Point2D and " + str (type (other)))

    def __rmul__ (self, other):
        if isinstance (other, numbers.Number):
            return Point2D (other * self.x, other * self.y)
        else:
            raise TypeError ("Cannot multiply " + str (type (other)) + " and Point2D")

    def __eq__ (self, other):
        return ((type (other) is Point2D) and
                self.x == other.x and self.y == other.y)


class PointPolar:
    def __init__ (self, x, y):
        self.set_polar (x, y)

    def set_polar (self, x, y):
        self.__r = sqrt (x*x + y*y)
        self.__theta = atan2 (y, x)

    def turn (self, phi):
        self.__theta += phi

    @property
    def x (self):
        return self.__r * cos (self.__theta)
    @property
    def y (self):
        return self.__r * sin (self.__theta)

    @x.setter
    def x (self, x):
        self.set_polar (x, self.y)
    @y.setter
    def y (self, y):
        self.set_polar (self.x, y)


