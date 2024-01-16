import java.util.Iterator;
import java.util.ListIterator;
import java.util.NoSuchElementException;

/**
 * Überarbeitung von MyList zur doppelt verketteten Liste.
 * Begonnen in der Vorlesung am 23.01.18,
 * !!!ZWISCHENSTAND Überarbeitung noch nicht fertig!!!
 *
 * @author Steffen Jost, Lehrstuhl TCS, LMU München
 * Created Di, 23.Januar 2018
 */

public class MyList2<E> implements MyListInterface<E> {

  private class Link<E> {
    E       data;
    Link<E> next;
    Link<E> prev;

    public Link() {
    }

    public Link(Link<E> prev, E data, Link<E> next) {
      this.data = data;
      this.next = next;
      this.prev = prev;
    }

    public E getData() {
      return data;
    }

    public void setData(E data) {
      this.data = data;
    }

    public Link<E> getNext() {
      return next;
    }

    public void setNext(Link<E> next) {
      this.next = next;
    }

    public Link<E> getPrev() {
      return prev;
    }

    public void setPrev(Link<E> prev) {
      this.prev = prev;
    }
  }

  // Instanzvariablen der Liste
  Link<E> first;
  Link<E> last;  // Klasseninvariante first==null impliziert last==null


  public MyList2() {
    first = null;
    last = null;
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
    Link<E> newSecond = first;
    this.first = new Link<>(null, e, newSecond);
    newSecond.setPrev(first);
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
   * @return Iterator über die Liste
   */
  @Override
  public Iterator<E> iterator() {
    return new MyIterator<>(this.first);
  }

  private class MyIterator<E> implements ListIterator<E> {

    Link<E> positionsZeiger;
    Link<E> letztGelesenesE;

    public MyIterator(Link<E> positionsZeiger) {
      this.positionsZeiger = positionsZeiger;
      this.letztGelesenesE = null;
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
      letztGelesenesE = positionsZeiger;
      positionsZeiger = positionsZeiger.getNext();
      return letztGelesenesE.getData();
    }




    //TODO ENDE ÜBERARBEITUNG IN DER VORLESUNG AM 23.01.18 //




    /**
     * Returns {@code true} if this list iterator has more elements when
     * traversing the list in the reverse direction.  (In other words,
     * returns {@code true} if {@link #previous} would return an element
     * rather than throwing an exception.)
     *
     * @return {@code true} if the list iterator has more elements when
     * traversing the list in the reverse direction
     */
    @Override
    public boolean hasPrevious() {
      return false;
    }

    /**
     * Returns the previous element in the list and moves the cursor
     * position backwards.  This method may be called repeatedly to
     * iterate through the list backwards, or intermixed with calls to
     * {@link #next} to go back and forth.  (Note that alternating calls
     * to {@code next} and {@code previous} will return the same
     * element repeatedly.)
     *
     * @return the previous element in the list
     * @throws NoSuchElementException if the iteration has no previous
     *                                element
     */
    @Override
    public E previous() {
      return null;
    }

    /**
     * Returns the index of the element that would be returned by a
     * subsequent call to {@link #next}. (Returns list size if the list
     * iterator is at the end of the list.)
     *
     * @return the index of the element that would be returned by a
     * subsequent call to {@code next}, or list size if the list
     * iterator is at the end of the list
     */
    @Override
    public int nextIndex() {
      return 0;
    }

    /**
     * Returns the index of the element that would be returned by a
     * subsequent call to {@link #previous}. (Returns -1 if the list
     * iterator is at the beginning of the list.)
     *
     * @return the index of the element that would be returned by a
     * subsequent call to {@code previous}, or -1 if the list
     * iterator is at the beginning of the list
     */
    @Override
    public int previousIndex() {
      return 0;
    }

    /**
     * Removes from the list the last element that was returned by {@link
     * #next} or {@link #previous} (optional operation).  This call can
     * only be made once per call to {@code next} or {@code previous}.
     * It can be made only if {@link #add} has not been
     * called after the last call to {@code next} or {@code previous}.
     *
     * @throws UnsupportedOperationException if the {@code remove}
     *                                       operation is not supported by this list iterator
     * @throws IllegalStateException         if neither {@code next} nor
     *                                       {@code previous} have been called, or {@code remove} or
     *                                       {@code add} have been called after the last call to
     *                                       {@code next} or {@code previous}
     */
    @Override
    public void remove() {

    }

    /**
     * Replaces the last element returned by {@link #next} or
     * {@link #previous} with the specified element (optional operation).
     * This call can be made only if neither {@link #remove} nor {@link
     * #add} have been called after the last call to {@code next} or
     * {@code previous}.
     *
     * @param e the element with which to replace the last element returned by
     *          {@code next} or {@code previous}
     * @throws UnsupportedOperationException if the {@code set} operation
     *                                       is not supported by this list iterator
     * @throws ClassCastException            if the class of the specified element
     *                                       prevents it from being added to this list
     * @throws IllegalArgumentException      if some aspect of the specified
     *                                       element prevents it from being added to this list
     * @throws IllegalStateException         if neither {@code next} nor
     *                                       {@code previous} have been called, or {@code remove} or
     *                                       {@code add} have been called after the last call to
     *                                       {@code next} or {@code previous}
     */
    @Override
    public void set(E e) {

    }

    /**
     * Inserts the specified element into the list (optional operation).
     * The element is inserted immediately before the element that
     * would be returned by {@link #next}, if any, and after the element
     * that would be returned by {@link #previous}, if any.  (If the
     * list contains no elements, the new element becomes the sole element
     * on the list.)  The new element is inserted before the implicit
     * cursor: a subsequent call to {@code next} would be unaffected, and a
     * subsequent call to {@code previous} would return the new element.
     * (This call increases by one the value that would be returned by a
     * call to {@code nextIndex} or {@code previousIndex}.)
     *
     * @param e the element to insert
     * @throws UnsupportedOperationException if the {@code add} method is
     *                                       not supported by this list iterator
     * @throws ClassCastException            if the class of the specified element
     *                                       prevents it from being added to this list
     * @throws IllegalArgumentException      if some aspect of this element
     *                                       prevents it from being added to this list
     */
    @Override
    public void add(E e) {

    }
  }

}
