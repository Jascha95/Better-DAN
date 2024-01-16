import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * Eine einfach verkettete generische Liste welche
 * das vereinfachte Interface MyListInterface implementiert.
 *
 * Listenglieder und Iterator sind private innere Klassen,
 * da deren Funktionalität eng mit der MyList-Klasse
 * verzahnt ist, aber irrelevant für einen externen Benutzer von MyList
 * (Es gibt keine public Methoden, welche Link nach Aussen herausgeben.)
 *
 * @author Steffen Jost, Lehrstuhl TCS, LMU München
 * Live programmiert in der Vorlesung am Di, 23.Januar 2018
 */

public class MyList<E> implements MyListInterface<E> {

  private static class Link<E> {
    E       data;
    Link<E> next;

    private Link() {
    }

    private Link(E data, Link<E> next) {
      this.data = data;
      this.next = next;
    }

    private E getData() {
      return data;
    }

    private void setData(E data) {
      this.data = data;
    }

    private Link<E> getNext() {
      return next;
    }

    private void setNext(Link<E> next) {
      this.next = next;
    }
  }

  // Instanzvariablen der Liste
  Link<E> first;


  public MyList() {
    first = null;
  }

  /**
   * @return Ist die Liste leer?
   */
  @Override
  public boolean isEmpty() {
    return first == null;
  }

  /**
   * @return Anzahl der Element in der Liste
   */
  @Override
  public int size() {
    int laenge = 0;
    Link<E> positionsZeiger = first;
    while(positionsZeiger != null){
      positionsZeiger = positionsZeiger.getNext();
      laenge++;
    }
    return laenge;
  }

  /**
   * @param e Neuer Kopf der Liste
   */
  @Override
  public void addFirst(E e) {
    this.first = new Link<>(e, this.first);
  }

  /**
   * @return Liefert den Kopf der Liste
   */
  @Override
  public E getFirst() {
    if (this.first == null) {
      // return null;
      throw new NoSuchElementException();
    }
    return this.first.getData();
  }

  /**
   * Schlägt der Liste den Kopf ab! >:)
   *
   * @return Kopf der Liste
   */
  @Override
  public E removeFirst() {
    if (this.first == null) {
      // return null;
      throw new NoSuchElementException();
    }
    E kopfDaten = first.getData();
    Link<E> zweites = first.getNext();
    first = zweites;
    return kopfDaten;
  }

  /**
   * @return Iterator über die Liste
   */
  @Override
  public Iterator<E> iterator() {
    return new MyIterator<>(this.first);
  }

  private class MyIterator<E> implements Iterator<E> {

    Link<E> positionsZeiger;

    private MyIterator(Link<E> positionsZeiger) {
      this.positionsZeiger = positionsZeiger;
    }

    /**
     * Returns {@code true} if the iteration has more elements.
     * (In other words, returns {@code true} if {@link #next} would
     * return an element rather than throwing an exception.)
     *
     * @return {@code true} if the iteration has more elements
     */
    @Override
    public boolean hasNext() {
      return (positionsZeiger != null);
    }

    /**
     * Returns the next element in the iteration.
     *
     * @return the next element in the iteration
     * @throws NoSuchElementException if the iteration has no more elements
     */
    @Override
    public E next() {
      if (positionsZeiger == null) {
        return null; // inkosistent mit getFirst(), zur Demonstration der Möglichkeiten
      }
      E ergebnis = positionsZeiger.getData();
      positionsZeiger = positionsZeiger.getNext();
      return ergebnis;
    }
  }

}
