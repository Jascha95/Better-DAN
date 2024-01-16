import java.util.Date;

public class Neben {

    public static void hauptEins(){
	System.out.println(new Date());
	Bankkonto konto = new Bankkonto();
        EinzahlenThread th1 = new EinzahlenThread(konto,100);
        Thread th2 = new AbhebenThread(konto,100);
        th1.start();
        th2.start();
    }
    public static void hauptZwei(){
	Bankkonto konto1 = new Bankkonto(1000);
	Bankkonto konto2 = new Bankkonto(1000);
	Bankkonto konto3 = new Bankkonto(1000);
        Ueberweiser th1 = new Ueberweiser(konto1,konto2,konto3,500);
	Ueberweiser th2 = new Ueberweiser(konto2,konto3,konto1,500);
        Ueberweiser th3 = new Ueberweiser(konto3,konto2,konto1,500);	
        th1.start();
        th2.start();
	th3.start();
    }
    public static void main(String[] args) {
	hauptZwei();
    }
}
class AbhebenThread extends Thread {
    private Bankkonto konto;
    private double betrag;
    final static int DELAY = 100;
    AbhebenThread(Bankkonto konto, double betrag) {
        this.konto = konto;
        this.betrag = betrag;   }
    public void run() {
        try {
            for (int i = 0; i <= 10000; i++) {
                if (isInterrupted())
                    throw new InterruptedException();
                konto.abheben(betrag);
                sleep(DELAY);
        }   }
        catch(InterruptedException e){}
    }}


class Ueberweiser extends Thread {
    private Bankkonto konto1;
    private Bankkonto konto2;
    private Bankkonto konto3;
    private double betrag;

    Ueberweiser(Bankkonto konto1,Bankkonto konto2,Bankkonto konto3,double betrag){
	this.konto1=konto1;this.konto2=konto2;this.konto3=konto3;
	this.betrag=betrag;
    }

    public void run(){
	while(true){
	    konto1.abheben(betrag);
	    konto2.abheben(betrag);
	    konto3.einzahlen(betrag);
	    konto3.einzahlen(betrag);
	}}}
	    
	    
class EinzahlenThread extends Thread {
    private Bankkonto konto;
    private double betrag;
    final static int DELAY = 100;
    EinzahlenThread(Bankkonto konto, double betrag) {
        this.konto = konto;
        this.betrag = betrag;   }
    public void run() {
        try {
            for (int i = 0; i <= 10000; i++) {
                if (isInterrupted()) 
                    throw new InterruptedException();
                konto.einzahlen(betrag);
                sleep(DELAY);
        }   }
        catch(InterruptedException e){}
}   }



class Bankkonto {
    private double kontostand;
    Bankkonto() {kontostand = 0;}
    Bankkonto(double st) {kontostand = st;}
    public  synchronized void einzahlen(double betrag) {
        System.out.println("Konto "+this+": Einzahlen von " + betrag);
        double neuerStand = kontostand + betrag;
        System.out.println("Konto "+this+": Neuer Kontostand: " + neuerStand);
        kontostand = neuerStand;notifyAll();}
    public synchronized void abheben(double betrag) {
	try{while (kontostand < betrag) {wait();}}
	catch(InterruptedException e){}
        System.out.println("Konto "+this+": Abheben von " + betrag);
        double neuerStand = kontostand - betrag;
        System.out.println("Konto "+this+": Neuer Kontostand: "+ neuerStand);
        kontostand = neuerStand;
}   }
