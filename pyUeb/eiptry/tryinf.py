class Fahrzeug():
    def __init__(self, Leergewicht, Baujahr):
        self.Leergewicht = Leergewicht #in kg
        self.Baujahr = Baujahr
    
    def getParameters(self) -> str: 
        return  "Leergewicht: " + repr(self.Leergewicht) +  " Baujahr: " + repr(self.Baujahr) 
    
    def __str__(self) -> str:
        return "Fahrzeug: " + self.getParameters() + " "
    
class Kraftfahrzeug(Fahrzeug): 
    def __init__(self,Leistung,Sitzplaetze,Stehplaetze, Leergewicht, Baujahr):
        self.Leistung = Leistung #in kilowatt
        self.Sitzplaetze = Sitzplaetze 
        self.Stehplaetze = Stehplaetze
        self.Leergewicht = Leergewicht 
        self.Baujahr = Baujahr 
  #      super().__init__(Leergewicht, Baujahr)

    def getParameters(self):
        return super().getParameters() + "Leistung: "+  repr(self.Leistung) + "Sitzplätze : " + repr(self.Sitzplaetze)

#################
# paras


    
    
   










class Bus(Kraftfahrzeug):
    def __init__(self, Leistung, Sitzplaetze, Stehplaetze):
        super().__init__(Leistung, Sitzplaetze, Stehplaetze)

class Fahrrad(Fahrzeug):
    def __init__(self, Rahmengroesse, Leergewicht, Baujahr):
        super().__init__(Leergewicht, Baujahr)
        self.Rahmengroesse = Rahmengroesse

class PKW(Kraftfahrzeug):
    def __init__(self, Leistung, Sitzplaetze, Stehplaetze):
        super().__init__(Leistung, Sitzplaetze, Stehplaetze)

class LKW(Kraftfahrzeug):
    def __init__(self, Leistung, Sitzplaetze, Stehplaetze):
        super().__init__(Leistung, Sitzplaetze, Stehplaetze)
###
#def __init__(self, angle:float= 180, radius:float=1, x:float=0, y:float=0)
#self.angle = angle 
#super().__init__(radius, x, y)