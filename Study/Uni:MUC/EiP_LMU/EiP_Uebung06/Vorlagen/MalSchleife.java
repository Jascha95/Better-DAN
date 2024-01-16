import java.awt.Point;
import java.awt.Color;

public class MalSchleife {

  /**
   * Vorlage für H5-1 "Mit Schleifen malen", H6-1, WiSe17/18, TCS, LMU München
   * @param args wird ignoriert
   */
  public static void main(String[] args) {
      for (ColoredPoint[] test : DemoInput.allTests) {
        GraphicsWindow window = zeichenSchleife(test);
        window.setText("Bitte klicken zum Fortfahren!");
        window.mouseClick(); // Warte auf Klick.
        window.killIn(3);
      }
      System.exit(0);
    }


    public static GraphicsWindow zeichenSchleife(ColoredPoint[] punkte){
      // Zuerst ein GraphicsWindow öffnen
      int xmax = 50; // Minimale x-Größe
      int ymax = 50; // Minimale y-Größe
      /*
       * TODO: Größe anpassen
       */
      GraphicsWindow graph = new GraphicsWindow(xmax,ymax);
      graph.setText("Wir malen in einer Schleife...");

      /*
       * TODO: Alle Punkte gemäß Aufgabenstellung einfärben
       */

      return graph;
    }

}
