import java.awt.*;
import java.util.ArrayList;

public class Demo {

  public static void main(String[] args) {
    ArrayList<HatFlaeche> welt = new ArrayList<>(3);
    welt.add(new Quadrat(new Point(4,7),3));
    welt.add(new Kreis(new Point(3,3),3));
    welt.add(new Quadrat(new Point(3,3),6));
    welt.add(new Kreis(new Point(2,2),2));

    ausgabe("a)", welt);

    // für Teilaufgabe ??
    Test kleinerSechzehn = new KleinerTest(16.0);
    ArrayList<HatFlaeche> welt2 = filter(kleinerSechzehn, welt);
    ausgabe("b)", welt2);

    // fuer Teilaufgabe ??
    ArrayList<HatFlaeche>  welt3 = filter(new Negation(kleinerSechzehn), welt);
    ausgabe("c)",welt3);

  }

  public static ArrayList<HatFlaeche> filter(Test test, ArrayList<HatFlaeche> aList) {
    // *** TODO ***
    return null;
    // *** TODO ***
	}

    public static void ausgabe(String label, ArrayList<HatFlaeche> dinge) {
    System.out.println(label);
	  System.out.println("Es gibt folgende Dinge:");
	  double flaeche = 0.0;
	  for (HatFlaeche gegenstand : dinge) {
	    System.out.println("  * " + gegenstand.toString());
	    flaeche += gegenstand.berechneFlaeche();
	  }
	  System.out.println("Mit Gesamtflaeche: " + flaeche);
  }
}
