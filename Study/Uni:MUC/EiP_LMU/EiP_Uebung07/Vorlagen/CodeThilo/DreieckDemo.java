import java.awt.*;
/*
 * Vorlage Übung A7-1 
 * "Einführung in die Programmierung" WS2017/18
 *
 * Lehrstuhl für Theoretische Informatik
 * LMU München
 *
 * Prof Martin Hofmann, Dr Steffen Jost
 */
 
public class DreieckDemo {

    public static void main(String[] args) {
        GraphicsWindow fenster = new GraphicsWindow(640,380);
        fenster.setText("Tannenbaum");
        Point spitze    = new Point (320,10);
        Point ecklinks  = new Point (120,100);
        Point eckrechts = new Point (520,100);
        Dreieck astspitze = new Dreieck(ecklinks,spitze,eckrechts);
        Dreieck astmitte  = new Dreieck(ecklinks,spitze,eckrechts);
        // Speicherbild1 hier anfertigen!
        astmitte.translate(0,60);
        // Speicherbild2 hier anfertigen!
        Dreieck astunten  = new Dreieck(ecklinks,spitze,eckrechts);
        astunten.translate(0,120);
        Dimension sockelwidth = new Dimension(40,40);
        Point sockelecke = new Point (300,220);
        Rectangle sockeloben = new Rectangle(sockelecke,sockelwidth);
        sockelecke.translate(0,40);
        Rectangle sockelmitte = new Rectangle(sockelecke,sockelwidth);
        Rectangle sockelunten = new Rectangle(sockelecke,sockelwidth);
        sockelunten.translate(0,40);

        fenster.setColor(Color.GREEN);
        astspitze.zeichneDichEin(fenster);
        astmitte.zeichneDichEin(fenster);
        astunten.zeichneDichEin(fenster);
        fenster.setColor(new Color(88, 36, 33));
        fenster.fill(sockeloben);
        fenster.fill(sockelmitte);
        fenster.fill(sockelunten);
        fenster.mouseClick();
        System.exit(0);
    }
}
