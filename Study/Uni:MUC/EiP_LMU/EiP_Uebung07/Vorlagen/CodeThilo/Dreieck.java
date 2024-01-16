import java.awt.*;

public class Dreieck {

    private Point a;
    private Point b;
    private Point c;

    public Dreieck(Point a, Point b, Point c) {
        this.a = a;
        this.b = b;
        this.c = c;
    }

    public void translate(int dx, int dy) {
        a.translate(dx,dy);
        b.translate(dx,dy);
        c.translate(dx,dy);
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
        Point[] poly = new Point [] { a,b,c};
        fenster.fillPoly(poly); 
    }
}
