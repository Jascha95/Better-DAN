import java.awt.*;

/************************************

> javac Main.java 
> java Main
Hallo!
Kontostand: 0.0€ von Max
Kontostand: 916.54€ von Steffen

*************************************/

public class Main {

    public static void main(String[] args) {
      // Zur Erinnerung: Hello World!
      System.out.println("Hallo!");

      // Zur Erinnerung: Rectangle-Demo aus Kapitel 1
      Rectangle box = new Rectangle(10,15,20,50);
      box.translate(20,0);
      box.x = box.x +20;

      // Hier geht der eigentliche Bankkonto Test los:
      Bankkonto steffensGiro = new Bankkonto("Steffen", 555);
      Bankkonto martinSpar   = new Bankkonto();

      steffensGiro.einzahlen(999.99);
      martinSpar.abheben(10000.0);
      steffensGiro.abheben(90.0);
      steffensGiro.einzahlen(1.0);

      //Ausgabe (alles in einer Zeile, d.h. Methodenaufrufe im Argument von println
      System.out.println("Kontostand: "+ martinSpar.getKontostand() + "€ von "+ martinSpar.getBesitzer());
      //Teile der Ausgabe zuerst in lokalen Variablen merken:
      double steffenKontostand = steffensGiro.getKontostand();
      String steffenBesitzer = steffensGiro.getBesitzer();
      System.out.print("Kontostand: ");
      System.out.println(""+ steffenKontostand + "€ von "+steffenBesitzer);
      // Kontostand sollte sein 916.54=999.99+5.55-90.0+1.0

    }
}
