class Movie():
    def __init__(self,title:str, year:int,rating:float):
        self.title = title
        self.year = year
        self.rating = rating
    
    def getParameters(self) -> str:
        x = self.__repr__(self.title) + self.__repr__(self.year) + self.__repr__(self.rating)
        return x 
    

    def __repr__(self):
        return "Movie(" + repr(self.title) + ', ' + repr(self.year) + ', ' + repr(self.rating) + ")"

##Movie.__str__ = movie_str überlegung wert ? no

    def avg_score(movies: list) -> float: 
        res = 0 
        for movie in movies:
            res += movie.rating
        return res/len(movies)

class Actors():
    def __init__(self,firstname:str,lastname:str,movie:list):
        self.firstname = firstname
        self.lastname = lastname
        self.movie = movie

    def __repr__(self): 
        return repr(self.firstname)+"," + repr(self.lastname)+"," + repr(self.movie)

    def worst_actor(actors:list) -> Actor:
        l = []


