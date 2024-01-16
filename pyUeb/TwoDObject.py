import math

class TwoDObject:
  def __init__(self, x=0, y=0):
    self.x = x
    self.y = y

  def move(self, xchange=0, ychange=0):
    self.x += xchange
    self.y += ychange

  def area(self):
    return 0


class Circle(TwoDObject):
  def __init__(self, radius=1, x=0, y=0):
    self.radius = radius
    self.x = x
    self.y = y

  def area(self):
    return self.radius * self.radius * math.pi

  def size_change(self, percent):
    self.radius *= (percent / 100.0)

class Sector (Circle):
  def __init__(self, angle= 180, radius=1, x=0, y=0):
    self.angle = angle
    self.radius = radius
    self.x = x
    self.y = y

  def area(self):
    return super().area() * self.angle / 360

# ----------------------------------------------

class Circle1(TwoDObject):
  def __init__(self, radius=1, x=0, y=0):
    self.radius = radius
    TwoDObject.__init__(self, x, y)


class Circle2(TwoDObject):
  def __init__(self, radius=1, x=0, y=0):
    self.radius = radius
    super().__init__(x, y)

# ----------------------------------------------

class Rectangle(TwoDObject):
  def __init__(self, height=1, width=1, x=0, y=0):
    self.height = height
    self.width = width
    super().__init__(x, y)

  def area(self):
    return self.height * self.width

  def size_change(self, percent):
    self.height *= (percent / 100.0)
    self.width  *= (percent / 100.0)


#class Square(Rectangle):
#  def __init__(self, x=0, y=0, side=1):
#    super().__init__(x, y, side, side)
# ----------------------------------------------
  
class TwoDObject1:
  def __init__(self, x=0, y=0):
    self.__x = x
    self._y = y

# ----------------------------------------------

class RectangleStretch(TwoDObject):
  def __init__(self, x=0, y=0, height=1, width=1):
    self.__height = height
    self.__width = width
    super().__init__(x, y)
    
  def stretch_height(self, percent):
    self.__height *= (percent / 100.0)

  def stretch_width(self, percent):
    self.__width *= (percent / 100.0)

  def area(self):
    return self.height * self.width

class SquareStretch(RectangleStretch):
  def __init__(self, x=0, y=0, side=1):
    super().__init__(x, y, side, side)

  def stretch_height(self, percent):
    super().stretch_height(self, percent)
    super().stretch_width(self, percent)

  def stretch_width(self, percent):
    super().stretch_width(self, percent)
    super().stretch_height(self, percent)

# ----------------------------------------------

class TwoDObjectS:
  def __init__(self, x=0, y=0):
    self.x = x
    self.y = y

  def __str__ (self):
    n = self.getName()
    p = self.getParameters()
    return n + "(" + p[1:] + ")"

  def getName (self):
    return "TwoDObject"

  def getParameters (self):
    return ",x=" + repr (self.x) + ",y=" + repr (self.y)

class CircleS(TwoDObjectS):
  def __init__(self, radius=1, **kwargs):
    self.radius = radius
    super().__init__ (**kwargs)

  def getName (self):
    return "Circle"

  def getParameters (self):
    return ",radius=" + repr(self.radius) + super().getParameters()

class SectorS(CircleS):
  def __init__(self, angle=180, **kwargs):
    self.angle = angle
    super().__init__ (**kwargs)

  def getName (self):
    return "Sector"

  def getParameters (self):
    return ",angle=" + repr (self.angle) + super().getParameters()

# ----------------------------------------------

class TwoDObjectCount:

  counter = 0
  
  def  __init__(self, x=0, y=0):
    self.x = x
    self.y = y
    TwoDObjectCount.counter = self.counter + 1