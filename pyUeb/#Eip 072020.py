class TwoDObject: 
    def __init__(self,x:float = 0, y : float = 0):
    	self.x =x
    	self.y = y 

    def move(self , xchange : float = 0, ychange :float = 0):
    	self.x = self.x + xchange
    	self.y = self.y + ychange

    def area(self):
    	return 0 

class Circle(TwoDObject):
	def __init__(self , radius:float=1, x:float=0, y:float=0):
		self.radius = radiusself.x = xself.y = y
	
	def area(self):
		return self.radius * self.radius * 3.14

	def size_change(self , percent:float):
		self.radius = self.radius * (percent / 100)
