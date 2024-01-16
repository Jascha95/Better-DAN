import java.awt.*;

public class Dreieck {

    private Position a;
    private Position b;
    private Position c;

    public Dreieck(Position a, Position b, Position c) {
        this.a = a;
        this.b = b;
        this.c = c;
    }

    public void translate(int dx, int dy) {
        this.a = a.translate(dx,dy);
        this.b = b.translate(dx,dy);
        this.c = c.translate(dx,dy);
    }

    public double berechneFlaeche() {
        final double edgeab = a.distance(b);
        final double edgebc = b.distance(c);
        final double edgeca = c.distance(a);
        final double umfang = (edgeab + edgebc + edgeca)/2;
        final double heron  = Math.sqrt(umfang * (umfang-edgeab)
                                               * (umfang-edgebc)
                                               * (umfang-edgeca));
        return  heron;
    }

    public void zeichneDichEin(GraphicsWindow fenster) {        
        // Da GraphicsWindow die Klasse Position nicht kennt,
        // und wir GraphicsWindow nicht verändern möchten,
        // müssen wir hier leider Punkte erzeugen:
        Point[] poly = new Point [] 
          {  new Point(a.getX(),a.getY())
          ,  new Point(b.getX(),b.getY())
          ,  new Point(c.getX(),c.getY())
          }; 
        fenster.fillPoly(poly); 
    }
}
