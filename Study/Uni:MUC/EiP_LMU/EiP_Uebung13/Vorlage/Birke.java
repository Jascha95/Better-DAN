import java.util.*;

/**
 * Eine mysterioese Mischung
 * Klasse für Uebungsaufgabe A13-3
 *
 * @author Birke Ballantz
 *
 */

public class Birke<E> implements Iterable<E> {

  private static class Link<E> {
    E data;
    Link<E> next;
    Link<E> prev;

    private Link(Link<E> prev, E data, Link<E> next) {
      this.data = data;
      this.next = next;
      this.prev = prev;
    }
  }

  // Instanzvariablen von Birke
  Link<E> first;
  Comparator<E> order;

  public Birke(Comparator<E> order) {
    first = null;
    this.order = order;
  }

  /**
   * @return Ist die Liste leer?
   */
  public boolean isEmpty() {
    return first == null;
  }

  /**
   * @return Anzahl der Element in der Liste
   */
  public int size() {
    return size(first);
  }

  private int size(Link<E> l) {
    if (l == null) {
      return 0;
    }
    return size(l.prev) + 1 + size(l.next);
  }

  public void insert(E e) {
    first = insert(e, first);
  }

  private Link<E> insert(E e, Link<E> l) {  //link:Vergleiche, e. Einfügen
    if (l == null) return new Link<E>(null, e, null);
    if (order.compare(e, l.data) <= 0) {  //Falls e< l
      l.prev = insert(e, l.prev);//füge ein in previuos, nach Bergleich mit previous
    } else {
      l.next = insert(e, l.next); //Sonst (e>=l) füge ein in next, nach vergleich mit next 
    }
    return l;
  }

  public boolean contains(E e) {
    return contains(e, first);
  }

  private boolean contains(E e, Link<E> l) {
    if (l == null) return false;
    int cmp = order.compare(e, l.data);
    if (cmp < 0) return contains(e, l.prev);
    if (cmp > 0) return contains(e, l.next);
    return true;
  }

  @Override
  public String toString() {
    return toString(first);
  }

  private String toString(Link<E> l) {
    if (l == null) return "";
    return "(" + toString(l.prev) + l.data + toString(l.next) + ")";
  }

  /**
   * @return Liefert den Kopf der Liste
   */

  public E getFirst() {
    if (first == null) throw new NoSuchElementException();
    return getFirst(first);
  }

  private static <E> E getFirst(Link<E> l) {
    if (l.prev == null) return l.data;
    return getFirst(l.prev);
  }

  public Iterator<E> sortedIterator() {
    return null; //TODO Ihre Aufgabe!
  }

  /**
   * @return Iterator über die Liste
   */
  public Iterator<E> iterator() {
    return new MyIterator(this.first);
  }

  private class MyIterator implements Iterator<E> {

    // Klasseninvariante: Elemente in position sind nie null!
    LinkedList<Link<E>> position;
    /* Hinweis: Hier reicht auch Interface "Queue", vgl. H12-1.
     *          Zur Lösung von A13-3d wird aber LinkedList benötigt!
     */
    public MyIterator(Link<E> positionsZeiger) {
      this.position = new LinkedList<>(); //Hinweis: bei Verwendung von Queue reicht hier ArrayDeque.
      addNonNull(positionsZeiger);
    }

    @Override
    public boolean hasNext() { return !position.isEmpty(); }

    @Override
    public E next() {
      if (position.isEmpty()) {
        throw new NoSuchElementException();
      }
      Link<E> l = position.remove();
      E ergebnis = l.data;
      addNonNull(l.prev);
      addNonNull(l.next);
      return ergebnis;
    }

    private void addNonNull(Link<E> l){
      if (l != null) position.add(l);
    }
  }
}
