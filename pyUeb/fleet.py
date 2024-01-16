##Leergewicht, Bau- jahr, Leistung, Sitzplätze, Stehplätze, Rahmengröße, Zuladung
    #ordnung



class Fahrzeug():
    def __init__(self, lg : int): #lg = Lehrgewicht
        self.lg = lg

class Kraftfahrzeug():
    def __init__(self, Leistung : int): #in Kilowatt also kw
        self.Leistung = Leistung

class Bus(Fahrzeug):
    def __init__(self, Baujahr : int, Sitzplaetze : int, Stehplaetze:int, **kwargs):
        self.Baujahr = Baujahr 
        self.Sitzplaetze = Sitzplaetze
        self.Stehplaetze = Stehplaetze 
    
        
class Fahrrad(Bus):
    def __init__(self, lg:int, Rahmengroesse : int, **kwargs):# in cm
        self.lg = lg 
        self.Rahmengroesse = Rahmengroesse 
        super().__init__(**kwargs) 
      

        

class PKW(Bus):
    def __init__(self, Baujahr: int, Sitzplaetze:int, **kwargs):
        self.Baujahr = Baujahr 
        self.Sitzplaetze = Sitzplaetze 
        super().__init__(**kwargs) 
        
class LKW(Bus):
    def __init__(self, Baujahr:int, Sitzplaetze:int,  Zuladung:int, **kwargs):
        self.Baujahr = Baujahr
        self.Sitzplaetze = Sitzplaetze
        self.Zuladung = Zuladung 
        super().__init__(**kwargs)



print("diesdas")

        
