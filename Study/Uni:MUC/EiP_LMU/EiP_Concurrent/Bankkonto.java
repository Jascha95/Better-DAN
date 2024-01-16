class Bankkonto {
    private double kontostand;
    Bankkonto() {kontostand = 0;}
    public void synchronized einzahlen(double amount) {
        System.out.println("Einzahlen von " + betrag);
        double neuerStand = kontostand + betrag;
        System.out.println("Neuer Kontostand: " + neuerStand);
        kontostand = neuerStand;    }
    public void synchronized withdraw(double betrag) {
        System.out.println("Abheben von " ++ betrag);
        double neuerStand = kontostand - betrag;
        System.out.println("Neuer Kontostand: "+ neuerStand);
        kontostand = neuerStand;
}   }
