

public class Bankkonto {

  // Instanzvariablen
  //
  // Beschreiben, welchen Zustand ein Objekt dieser Klasse hat, d.h.
  // alle "Schubfächer" der "Box" mit ihren Typen
  private double kontostand;
  private String besitzer = "Martin";


  // Konstruktoren
  //
  // Werden mit new aufgerufen; der Konstruktor wird
  // anhand der Parameter idenfiziert

  // Konstruktor ohne Parameter
  Bankkonto() {
    besitzer = "Max";
    //TODO: Bonität prüfen
  }

  // Konstruktor mit einem String Parameter
  Bankkonto(String besitzer) {
    this.besitzer = besitzer;
  }

  // Konstruktor mit String und int als Parameter in dieser Reihenfolge
  Bankkonto(String besitzer, int startWertInCent){
    this.besitzer = besitzer;
    this.kontostand = startWertInCent / 100.0;
  }


  // Methoden der Klasse

  // getter für Instanzvariable besitzer
  public String getBesitzer() {
    return this.besitzer;
  }

  // setter für Instanzvariable besitzer
  public void setBesitzer(String besitzer) {
    this.besitzer = besitzer;
  }

  // Kontostand auslesen
  public double getKontostand(){
    if (kontostand < 0) {
      return 0;
    }
    // Hier kommen wir nur hin, wenn kontostand ≥ 0
    return kontostand;
  }


  // Einzahlen, nur für positive Beträge
  public void einzahlen(double betrag){
    if (betrag > 0) {
      kontostand = kontostand + betrag;
    } else {
      double diff = kontostand - betrag; // lokale Variable ohne Bedeutung, nur zur Demonstration lokaler Variablen
      System.out.println("Differenz wäre: " + diff);
      {
        System.out.println("Differenz wäre: " + diff);
      }
    } // Lebensspanne der lokalen Variable diff endet hier

    // System.out.println("Differenz wäre: " + diff); // diff hier unbekannt

  }

  // Einzahlen von Cent-Beträgen als int
  public void einzahlenCent(int betrag){
    if (betrag > 0) {
      kontostand = kontostand + ((double) betrag / 100.0);
    }
  }

  // Abheben
  public void abheben(double betrag) {
    kontostand = kontostand - betrag;
  }


}
