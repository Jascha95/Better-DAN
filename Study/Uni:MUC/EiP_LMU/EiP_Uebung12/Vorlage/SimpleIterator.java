import java.util.Iterator;

/**
 * Einfaches Interface eines Iterators.
 * Das Interface ist identisch zu Iterator,
 * wir geben es hier nur an, damit das Beispiel vollständig ist, d.h.
 * man könnte hier "extends Iterator<E>" entfernen und ebenso
 * im Interface SimpleList "extends Iterable<E>".
 * Einziger Nachteil: "for-each"-Schleifensyntax wäre dann nicht mehr erlaubt.
 *
 * Vorlesung "Einführung in die Programmierung", W17, LMU München
 *
 * @author jost
 * Created Fr, 19.Januar 2018
 */

public interface SimpleIterator<E> extends Iterator<E> {

  //Werden schon so von Iterator definiert, d.h. könnte man auch auskommentieren:
  boolean hasNext();
  E next();

  /* Man könnte als Übung noch hinzufügen:
  void remove();
  void add(E element);
  void set(E element);
  E previous();
  ...
  */
}
