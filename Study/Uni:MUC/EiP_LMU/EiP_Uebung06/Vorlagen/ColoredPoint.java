import java.awt.Color;
import java.awt.Point;

public class ColoredPoint {
  /**
   * Eine unveränderliche (immutable) Klasse für
   * gefärbte Punkte in der zweidimensionalen Ebene.
   * @param x Horizontale Position des Punktes
   * @param y Vertikale Position des Punktes
   * @param color Färbung des Punktes
   */

  private final Color color;   // Okay, da java.awt.Color immutable ist.
  // private final Point point; // Zu gefährlich, da Point veränderlich ist!
  private final int x;
  private final int y;

  public ColoredPoint(int x, int y, Color color) {
    this.x = x;
    this.y = y;
    this.color = color;
    // Hier könnte noch mehr Code zur
    // Initalisierung stehen, für diese
    // simple Klasse nicht notwendig
  }

  public ColoredPoint(int x, int y, int color) {
    this(x,y, new Color(color));            // anderen Konstruktor ausführen
  }

  public ColoredPoint(Point point, Color color) {
    this(point.x, point.y, color);          // anderen Konstruktor ausführen
  }

  public ColoredPoint(Point point, int color) {
    this(point, new Color(color));          // anderen Konstruktor ausführen
  }

  public int getX() {
    return x;
  }

  public int getY() {
    return y;
  }

  public Color getColor() {
    return color;
  }

  /**
   * Da java.awt.Point verändert werden kann,
   * erzeugen wir hier immer einen frischen Punkt
   * @return Point an den gleichen Koordinaten
   */
  public Point getPoint() {
    return new Point(this.x,this.y);
  }

}
