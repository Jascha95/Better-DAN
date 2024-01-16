class Sparkonto extends Bankkonto {}



public class Bankkonto<T> implements Comparable<T extends Bankkonto>{
    public static void main(){
	Comparable<Sparkonto> z = new Sparkonto();
    }
    private double kontostand;

    public int compareTo(Bankkonto x) {
	if (kontostand < x.kontostand) return -1;
	if (kontostand == x.kontostand) return 0;
	return 1;
    }
    public void einzahlen(double betrag) {
	kontostand = kontostand + betrag;
    }
    public void abheben(double betrag) {
	kontostand = kontostand - betrag;
    }
    public double getKontostand()  {
	return kontostand;
    }
    public void ueberweisen(double betrag, Bankkonto empfaenger) {
	kontostand = kontostand - betrag; 
	empfaenger.einzahlen(betrag);
    }
    
 

    public Bankkonto() {
	kontostand = 0.0;
    }
   
}
