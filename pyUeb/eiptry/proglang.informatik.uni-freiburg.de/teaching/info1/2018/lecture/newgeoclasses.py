class TwoDObject:

    counter = 0
  
    def __init__(self, x=0, y=0):
        self.x = x
        self.y = y
        TwoDObject.counter +=1

    def move(self, xchange=0, ychange=0):
        self.x += xchange
        self.y += ychange

    def position(self):
        return(self.x, self.y)

    
class Circle(TwoDObject):
    def __init__(self, radius=1, **kwargs):
        self.radius = radius
        super().__init__(**kwargs)

    def area(self):
        return self.radius * self.radius * 3.14

    def size_change(self, percent):
        self.radius *= (percent / 100.0)



class Rectangle(TwoDObject):
    def __init__(self, height=1, width=1, **kwargs):
        self._height = height
        self._width = width
        super().__init__(**kwargs)
    
    def stretch_height(self, percent):
        self._height *= (percent / 100.0)

    def stretch_width(self, percent):
        self._width *= (percent / 100.0)

    def size_change(self, percent):
        self.stretch_width(percent)
        self.stretch_height(percent)

    def area(self):
        return self._height * self._width


class CompositeObject(TwoDObject):

    def __init__(self, objs=(), **kwargs):
        super().__init__(None, None)
        self.objects = list(objs)

    def add(self, obj):
        self.objects.append(obj)

    def rem(self, obj):
        self.objects.remove(obj)

    def size_change(self, percent):
        for obj in self.objects:
            obj.size_change(percent)

    def move(self, xchange, ychange):
        for obj in self.objects:
            obj.move(xchange, ychange)

    def position(self):
        return self.objects[0].position() if self.objects else None

    

if __name__ == '__main__':
    c = Circle(x=1,y=2)
    r = Rectangle(10,10)
    a = CompositeObject((r,c))
    a.size_change(200)
    print(r.area())
    a.move(40,40)
    print(a.position())
    print(c.position())
    b = CompositeObject()
    a.add(b)
    a.move(-10, -10)
