#Leergewicht, Bau- jahr, Leistung, Sitzplätze, Stehplätze, Rahmengröße, Zuladung
    #ordnung
class Fahrzeug():
    def __init__(self, lg=0, Baujahr=0, Rahmengroesse=0): #lg = Lehrgewicht
        self.lg = lg
        self.Baujahr = Baujahr
        self.Rahmengroesse = Rahmengroesse
    
    
    def Baujahr(self):
        return super().Baujahr()
        
    

class Kraftfahrzeug(Fahrzeug):
    def __init__(self, Leistung, Sitzplaetze, Stehplaetze, Zuladung,**kwargs): #in Kilowatt also kw
        self.Leistung = Leistung
        self.Sitzplaetze = Sitzplaetze
        self.Stehplaetze = Stehplaetze
        self.Zuladung = Zuladung
        super().__init__(**kwargs) 
#__________________________________________________
#class Bus(Kraftfahrzeug): 
    #def __init__(self):
class Fahrrad(Fahrzeug):
    def __init__(self, **kwargs):#TypeError: __init__() should return None, not 'tuple'
       # self.Baujahr = Baujahr                     #def __init__(self,Baujahr, lg,  Rahmengroesse=0):
       # self.lg = lg
       # self.Rahmengroesse = Rahmengroesse
        super().__init__(**kwargs)
        return Baujahr, lg, Rahmengroesse
    print("Baujahr"+ "  kg"+ " cm")
    
    def dofr(self, Fahr):
        self.Fahr = Fahr


#class PKW(Kraftfahrzeug):
  #  def __init__(self):


#class LKW(Kraftfahrzeug):
  #  def __init__(self):

    
