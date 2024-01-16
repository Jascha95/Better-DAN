/**
 * Sehr einfach Interface für Listen, i.e.
 * "geordneten Folgen von Elementen mit gleichem Typ"
 * wobei der Typ hier beliebig ist.
 *
 * "extends Iterable<E>" ist eigentlich redundant wegen Zeile 23,
 * erlaubt aber die Verwendung der "for-each"-Schleifensyntax
 * wie in LinkedListDemo, Zeile 26 gezeigt.
 *
 * Vorlesung "Einführung in die Programmierung", W17, LMU München
 *
 * @author jost
 * Created Fr, 19.Januar 2018
 */

public interface SimpleList<E> extends Iterable<E> {

  /**
   * @return true, falls die Liste leer ist.
   */
  boolean isEmpty();
  /**
   * @return Anzahl Elemente in der Liste
   */
  int size();

  /**
   * @throws IndexOutOfBoundsException für ungültige Indices
   * @param index
   * @return Element am Index in der Liste
   */
  E get(int index);

  /**
   * Ersetzen eines Elementes an einer vorhandenen Position
   * @throws IndexOutOfBoundsException für ungültige Indices
   * @param index
   * @param element Neues Element
   * @return Altes Element
   */
  E set(int index, E element);

  /**
   * Neues element als Kopf der Liste einfügen
   * @param element
   */
  void addFirst(E element);

  /**
   * Kopf der Liste entfernen
   * @return ehemaliges Kopf-Element
   */
  E removeFirst();







  /**
   * @return Iterator zur Verarbeitung der Listenelemente in gegebener Reihenfolge
   */
  SimpleIterator<E> iterator();

    //zirück
  
  


}
