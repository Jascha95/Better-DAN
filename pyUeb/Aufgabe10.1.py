# Aufgabe 10.1
def powerset(n: int, s: list) -> list:  
	"""Definieren Sie eine Funktion powerset(s: list) -> list,
	die für eine beliebige Menge s, rekursiv die Potenzmenge berechnet 
	und zurückgibt. VerwendenSie dabei 
	Python-Listen zur Darstellung von Mengen. 
	Wir nehmen an, dasskeine Wiederholungen in den 
	Eingabelisten auftreten. 
	Beispiel:>>> powerset([1, 2, 3])[[], 
	[1], [2], [3], [1, 2], [1, 3], [2, 3], [1, 2, 3]]
	"""
	s = [] 
	while n != 0:
		if n % 2 == 0:
			n, s = (n*n, n//2, s)
		# else:# n x, n, acc = (x*x, n//2, acc*x)
	else:
		return s 


	def power_acc (x, n, acc):
		if n==0: 
			return acc 
		else:returnpower_acc (x, n-1, acc * x)
























