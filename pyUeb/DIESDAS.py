##Leergewicht, Bau- jahr, Leistung, Sitzplätze, Stehplätze, Rahmengröße, Zuladung
    #ordnung



class Fahrzeug():
    def __init__(self, lg : int): #lg = Lehrgewicht
        self.lg = lg

class Kraftfahrzeug():
    def __init__(self, Leistung : int): #in Kilowatt also kw
        self.Leistung = Leistung

class Bus():
    def __init__(self, Baujahr : int, Sitzplaetze : int, Stehplaetze:int):
        self.Baujahr = Baujahr 
        self.Sitzplaetze = Sitzplaetze
        self.Stehplaetze = Stehplaetze 
    
        
class Fahrrad(Bus):
    def __init__(self, Rahmengroesse : int, **kwargs):# in cm
        self.Rahmengroesse = Rahmengroesse 
        super().__init__(**kwargs) 
        

class PKW(Bus):
    def __init__(self, **kwargs):
        super().__init__ (**kwargs) 
        
class LKW():
    def __init__(self, Zuladung:int, **kwargs):
        self.Zuladung = Zuladung 
        super().__init__(**kwargs)
        
