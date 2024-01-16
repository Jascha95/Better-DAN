import java.util.Iterator;

/**
 * Stark vereinfachte ListenInterface.
 *
 * In der Vorlesung werden wir dieses Interface schrittweise erweitern,
 * um uns unserem Ziel, dem Interface LinkedList aus der Standardbibliothek
 * anzunähern.
 *
 * Das Interface erweitert das Interface Iterable aus der Standardbibliothek
 * damit wir die for-each-Schleifen-Syntax verwenden können.
 *
 * @author Steffen Jost, Lehrstuhl TCS, LMU München
 * Live programmiert in der Vorlesung am Di, 23.Januar 2018
 */

public interface MyListInterface<E> extends Iterable<E> {
  /**
   * @return Ist die Liste leer?
   */
  public boolean isEmpty();

  /**
   *
   * @return Anzahl der Element in der Liste
   */
  public int size();

  /**
   *
   * @param e Neuer Kopf der Liste
   */
  public void addFirst(E e);

  /**
   * @return Liefert den Kopf der Liste
   */
  public E getFirst();

  /**
   * Schlägt der Liste den Kopf ab! >:)
   * @return Kopf der Liste
   */
  public E removeFirst();


  // Folgende Zeilen sind unnötig, da wir dieses Interface
  // ohnehin durch "extends Iterable<E>" am Anfang haben.
  // Eine Wiederholung liefert aber keinen Fehler,
  // also wiederholen wir es zur Klarheit:
  /**
   *
   * @return Iterator über die Liste
   */
   public Iterator<E> iterator();

}
