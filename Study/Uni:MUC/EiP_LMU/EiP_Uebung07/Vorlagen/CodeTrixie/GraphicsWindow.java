// package de.lmu.tcs;

import javax.swing.*;
import java.awt.*;
import java.awt.event.*;
import java.awt.geom.GeneralPath;
import java.awt.geom.RectangularShape;
import java.util.ArrayList;

/**
 Eine Klasse zu pädagogischen Zwecken.
 Erlaubt die Eingabe von Punktkoordinaten
 mittels Mausklicks, das Zeichnen einfacher
 2D Objekte (java.awt.Shape), sowie die
 Ausgabe von Texten in einer Statuszeile.
 @version 3.05
 @author Martin Hofmann und die EiP-Teams verschiedener Jahre
 */

public class GraphicsWindow {

  private int width;
  private int height;
  private JFrame dasFenster;
  private static int fensterZahl;
  private static int fensterNr;
  private Label label;
  private GraphicsWindowPanel panel;
  private Point mousePos;
  private Color activeColor = Color.BLACK;
  final private Color backColor = Color.WHITE;
  MyMouseAdapter mouseListener;

  /**
   Erzeugt ein Fenster der Größe 640 auf 480 mit Textausgabe, Mauseingabe und Grafikausgabe.
   */
  public GraphicsWindow() {
    this(640, 480);
  }

  /**
   Erzeugt ein Fenster in vorgegebener Größe mit Textausgabe, Mauseingabe und Grafikausgabe.
   @param width Breite des Fensters
   @param height Höhe des Fensters
   */
  public GraphicsWindow(int width, int height) {
    this.width  = width;
    this.height = height;
    dasFenster  = new JFrame();
    dasFenster.setTitle("Grafikfenster " + ++fensterNr);
    fensterZahl++;
    dasFenster.setLocationByPlatform(true);
    dasFenster.setSize(width,height+50);
    dasFenster.getContentPane().setPreferredSize(new Dimension(width, height+50));
    dasFenster.pack();
    dasFenster.addWindowListener(new WindowAdapter(){
      public void windowClosing(WindowEvent e) {
        dasFenster.dispose(); // nicht gleich alle Fenster abschiessen
        if (--fensterZahl<1) System.exit(0);
      }
    });

    label = new Label("Statuszeile...");
    label.setFont(new Font("Helvetica", Font.PLAIN, 12));
    dasFenster.getContentPane().add(label,"North" );
    panel = new GraphicsWindowPanel();
    panel.setBackground(Color.white);
    panel.addCommand(new SetColor(activeColor));
    dasFenster.getContentPane().add(panel,"Center");
    mousePos = new Point();
    mouseListener = new MyMouseAdapter();
    panel.addMouseListener(mouseListener);
    clear();
    dasFenster.setVisible(true);
  }

  /**
   Gibt eine Zeichenkette oben im Fenster aus.
   @param text diese Zeichenkette
   */
  public void setText(String text) {
    label.setText(text);
  }
  /**
   Liest den oben im Fenster angezeigten Text aus.
   @return den Text
   */
  public String getText() {
    return label.getText();
  }
  /**
   Wartet auf einen Mausklick. Die Methode blockiert das
   aufrufende Programm solange bis der Mausklick erfolgt ist.
   @return die Koordinaten des angeklickten Punkts
   */

  public Point mouseClick() {
    try{
      synchronized(mouseListener){mouseListener.wait();}
    }
    catch(InterruptedException e){
      e.printStackTrace();
    }
    return mousePos;
  }

  class MyMouseAdapter extends MouseAdapter {

    /**
     Beendet das Warten auf den Mausklick und verwertet die Koordinaten.
     Diese Methode ist nicht für den Anwender bestimmt.
     */

    synchronized public void mouseClicked(MouseEvent e){
      mousePos = e.getPoint();
      notifyAll();
    }
  }


  /**
   Schaltet die Zeichenfarbe auf die Hintergrundfarbe um. Dies ist
   das Mittel, um gezeichnete Linien wieder zu löschen.
   */
  public void switchToBackgroundColor(){
    activeColor = backColor;
    panel.addCommand(new SwitchToBackgroundColor(activeColor));
    panel.repaint();
  }

  /**
   Schaltet die Zeichenfarbe auf Schwarz um.
   */
  public void switchToForegroundColor(){
    activeColor = Color.BLACK;
    panel.addCommand(new SetColor(activeColor));
    panel.repaint();
  }


  /** Liefert die aktuelle Zeichenfarbe.
   @return die aktuelle Zeichenfarbe des GraphicsWindow. */
  public Color getColor() {
    // return panel.getGraphics().getColor(); // getGraphics() has unpleasant side-effects. :(
              /* Fixed by adding another instance variable activeColor for now. */
    return activeColor;
  }

  /**
   Zeichnet eine Linie in der aktuellen Zeichenfarbe.
   @param x Anfangspunkt
   @param y Endpunkt
   */
  public void drawLine(Point x, Point y){
    // Odering points reduces the amount of graphical artifacts in rendering the same object in different ways
    Point x1 = x;
    Point y1 = y;
    if ((x.x > y.x) || ((x.x == y.x) && (x.y > y.y))) {
      x1 = y;
      y1 = x;
    }
    panel.addCommand(new DrawLine(x1,y1));
    panel.repaint();
  }

  /**
   Zeichnet einen Punkt in der aktuellen Zeichenfarbe.
   @param p Punkt
   */
  public void drawPoint(Point p){
    drawLine(p, p);
  }

  /**
   Zeichnet einen Punkt in der aktuellen Zeichenfarbe.
   @param p Punkt
   */
  public void drawStringAt(String s, Point p){
    Command c = new DrawString(s,p);
    panel.addCommand(c);
    panel.repaint();
  }

  /**
   * Zeichnet ein Polygon, gegeben durch ein geordnetes Array von Punkten
   * @param poly Array von Punkten, welche 2D Polygon beschreiben.
   */
  public void drawPoly(Point[] poly) {
    Point previous = poly[poly.length-1];
    for (Point next : poly) {
      drawLine(previous,next);
      previous = next;
    }
  }

  /**
   * Zeichnet und füllt ein Polygon, gegeben durch ein geornetes Array von Punkten
   * @param poly Array von Punkten, welche 2D Polygon beschreiben.
   */
  public void fillPoly(Point[] poly) {
    int[] xpoints = new int[poly.length];
    int[] ypoints = new int[poly.length];
    for (int i = 0; i<poly.length; i++) {
      xpoints[i] = poly[i].x;
      ypoints[i] = poly[i].y;
    }
    Polygon polygon = new Polygon(xpoints,ypoints,poly.length);
    fill(polygon);
  }


  /**
   Zeichnet ein geometrisches Objekt.
   */
  public void draw(Shape s) {
    panel.addCommand(new Draw(s));
    panel.repaint();
  }

  /**
   Füllt ein geometrisches Objekt aus.
   */
  public void fill(Shape s) {
    panel.addCommand(new Fill(s));
    panel.repaint();
  }

  /** Das aufrufende Programm wird für ein gegebene Zeitspanne blockiert.
   @param millis Die Zeitspanne in Millisekunden*/
  public void sleep(long millis) {
    try {Thread.sleep(millis);} catch (Exception e){}
  }

  /** Setzt die Zeichenfarbe. */
  public void setColor(Color d) {
    activeColor = d;
    panel.addCommand(new SetColor(activeColor));
    panel.repaint();
  }

  /**
   Setzt die Zeichenfarbe auf einen Grauwert
   @param shade Grauwert zwischen 0(schwarz) und 255(weiß)
   */
  public void setGrayColor(int shade) {
    setColor(new Color(shade, shade, shade));
  }

  /**
   Setzt die Zeichenfarbe für die Mandelbrot-Aufgabe
   @param n Anzahl der Iterationen, die durch die Farbe symboliziert werdem soll
   */
  public void setMandelColor(int n) {
    float r = (float) Math.min(1.0,((double) n / 9.0) );
    float g = (float) Math.min(1.0,((double) n / 99.0) );
    float b = (float) Math.min(1.0,((double) n / 999.0) );
    setColor(new Color(r, g, b));
  }

  /** Löscht das Bild */
  public void clear() {
//        Color oldActive = activeColor;
    panel.clearAll();
//        this.switchToBackgroundColor();
//        fill(new Rectangle(0,0,width,height));
//        setColor(oldActive);
  }

  public void killIn(int secs) {
    Timer t  = new Timer(1000*secs, new ActionListener(){
      @Override
      public void actionPerformed(ActionEvent e) {dasFenster.dispose();}
    }
    );
    t.setRepeats(false);
    t.start();
  }
}




class GraphicsWindowPanel extends JPanel
{
  private static final long serialVersionUID = 1L;
  private ArrayList<Command> cl = new ArrayList<Command>();

  public void paintComponent(Graphics g)
  {
    super.paintComponent(g);
    Graphics2D g2D = (Graphics2D)g;

    ArrayList<Command> cl = this.cl; // Kopie wegen Nebenläufigkeit von Swing
    int size = cl.size();
    for (int i=0; i<size; i++) {
      Command c = cl.get(i);
      if (c != null) c.execute(g2D);
    }
  }

  void addCommand(Command c)
  {
    cl.add(c);
  }

  void clearAll()
  {
//              try {
//                      SwingUtilities.invokeAndWait(new Runnable() {
//                              @Override
//                              public void run() {
    cl = new ArrayList<Command>();
//                              }
//                      });
//              } catch (InterruptedException e) {
//                      // TODO Auto-generated catch block
//                      e.printStackTrace();
//              } catch (InvocationTargetException e) {
//                      // TODO Auto-generated catch block
//                      e.printStackTrace();
//              }
  }
}


abstract class Command //implements Serializable
{
  abstract  void execute(Graphics2D g2D);

  /** Clone a shape. This method is needed because Shape
   * does not define clone(), although many shape classes do.
   * Kopiert aus jsky-2.6 auf ftp.eso.org */
  static Shape cloneShape(Shape s) {
    // FIXME Add more specific shapes
    if (s instanceof RectangularShape) {
      return (RectangularShape) ((RectangularShape) s).clone();
    } else {
      return new GeneralPath(s);
    }
  }

}

class DrawLine extends Command {
  Point von;
  Point bis;
  DrawLine(Point von, Point bis) {
                /* Clonen der Punkte essentiell um Aliasingeffekte beim Redraw zu verhindern */
    this.von = new Point(von);
    this.bis = new Point(bis);
  }
  void execute(Graphics2D g2D)
  {
    g2D.drawLine(this.von.x,this.von.y,this.bis.x,this.bis.y);
  }
}

class SwitchToForegroundColor extends Command {
  SwitchToForegroundColor() {}
  void execute(Graphics2D g2D) {
    g2D.setColor(Color.black);
  }
}

class SwitchToBackgroundColor extends Command {
  Color backcolor;
  SwitchToBackgroundColor(Color backcolor) {this.backcolor = backcolor;}
  void execute(Graphics2D g2D) {
    g2D.setColor(backcolor);
  }
}

class SetColor extends Command {
  Color color;
  SetColor(Color color) {this.color = color;}
  void execute(Graphics2D g2D) {
    g2D.setColor(this.color);
  }
}


class Draw extends Command {
  Shape shape;
  Draw(Shape shape) {this.shape = cloneShape(shape);}
  void execute(Graphics2D g2D) {
    g2D.draw(this.shape);
  }
}

class Fill extends Command {
  Shape shape;
  Fill(Shape shape) {this.shape = cloneShape(shape);}
  void execute(Graphics2D g2D) {
    g2D.fill(this.shape);
  }
}

class DrawString extends Command {
  String string;
  Point position;
  DrawString(String string, Point position) {this.string = string; this.position = position;}
  @Override
  void execute(Graphics2D g2D) {
    g2D.drawString(string, position.x, position.y);
  }
}

