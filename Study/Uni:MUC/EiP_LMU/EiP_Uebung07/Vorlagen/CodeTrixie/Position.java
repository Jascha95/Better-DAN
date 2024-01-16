import java.util.ArrayList;

/**
 * Unveränderliche Klasse für zweidimensionale Positionen
 *
 * Zugehörigkeit: Modell
 *
 * @author jost
 * Created Di, 28.November 2017
 */

public class Position {

  private final Integer x;
  private final Integer y;

  public Position(int x, int y) {
    this.x = x;
    this.y = y;
  }

  public int getX() {
    return x;
  }

  public int getY() {
    return y;
  }

  public ArrayList<Position> getNachbarPositionen(){
    ArrayList<Position> nachbarn = new ArrayList<>(8);
    for (int hv=-1;hv<=1;hv++){
      for (int vv=-1;vv<=1;vv++) {
        if (!(hv==0 && vv==0)) {
          nachbarn.add(new Position(this.x + hv, this.y + vv));
        }
      }
    }
    return nachbarn;
  }

  public Position translate(int dx, int dy){return new Position(this.x+dx, this.y+dy);}
  
  public double distance(Position other){
  return Math.sqrt(Math.pow(this.x - other.x,2) + Math.pow(this.y - other.y,2)); }


  /* Automatisch von der IDE (hier Intellij) erzeugter Code,
   * den wir für dieses Beispiel gar nicht benötigen!
   * Sie können das Folgende also gefahrlos ignorieren!
   *
   * Dieser Code legt fest, wie equals funktioniert.
   * Leider können wir diesen Code erst später in der
   * Vorlesung verstehen; bis jetzt wissen wir nur,
   * welchen Zweck die Methode equals hat (siehe Folien 2.46 ff.).
   *
   * Für dieses Beispiel benötgen wir jedoch kein equals.
   * Wir haben es dennoch definiert, weil es guter Stil ist,
   * bei solchen unveränderlichen Klassen equals sinnvoll festzulegen.
   * Außerdem war es ja nur ein Klick/Tastendruck in der IDE.
   */
  @Override
  public boolean equals(Object o) { // Automatisch von IDE
    if (this == o) return true;
    if (o == null || getClass() != o.getClass()) return false;

    Position position = (Position) o;

    if (x != null ? !x.equals(position.x) : position.x != null) return false;
    return y != null ? y.equals(position.y) : position.y == null;
  }
  @Override
  public int hashCode() { // Automatisch von IDE
    int result = x != null ? x.hashCode() : 0;
    result = 31 * result + (y != null ? y.hashCode() : 0);
    return result;
  }
}
