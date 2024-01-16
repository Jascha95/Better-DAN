import tkinter as tk
import sys
import time

class TwoDObject:
  def __init__(self, cv, x=0, y=0):
    assert type(cv) == tk.Canvas, "Must provide a canvas to constructor"
    self.cv = cv
    self.x = x
    self.y = y
    self.index = 0

  def move(self, xchange=0, ychange=0):
    self.x += xchange
    self.y += ychange
    cv.move(self.index, xchange, ychange)

class Circle(TwoDObject):
  def __init__(self, radius=1, **kwargs):
    self.radius = radius
    super().__init__(**kwargs)
    self.index = self.cv.create_oval(self.x-self.radius,
                                self.y-self.radius,
                                self.x+self.radius,
                                self.y+self.radius)


  def size_change(self, percent):
    self.radius *= (percent / 100)
    self.cv.delete(self.index)
    self.index = self.cv.create_oval(self.x-self.radius,
                                self.y-self.radius,
                                self.x+self.radius,
                                self.y+self.radius)

root = tk.Tk()
cv = tk.Canvas(root, height=600, width=600)
cv.pack()

c1 = Circle(cv=cv, x=100, y=100, radius= 30)
c2 = Circle(cv=cv, x=300, y=300, radius= 50)
cv.update()
time.sleep(2)
c2.size_change(50)
cv.update()
time.sleep(2)
c3 = Circle(cv=None)


if "idlelib" not in sys.modules:
    root.mainloop()



                                
