import java.util.ListIterator;
import java.util.NoSuchElementException;

/**
 * Variante von MyList2 (doppelt verkettete Liste)
 * mit dynamischer innerere Klasse MyIterator.
 * (Die explizite Instanzvariable mit Verweis auf das MyList3 Objekt
 *  entfällt dann, da diese dann implizit vorhanden ist.
 *  Man argumentieren, dass diese Variante schlechter lesbar & verstehbar ist.
 *  Bei nicht-statischen Link-Objekten würde auch ein Problem entstehen,
 *  wenn man Methoden hätte, welche Link-Objekte zwischen verschiedenen Listen
 *  austauschen dürfen.
 *
 *  Hinweis: Das Problem in der Vorlesung "Type Link<E> incomaptible with Link<E>"
 *    lag an einer Überschattung der Typvariablen E aus MyList3 mit der Typvariablen
 *    gleichen Names MyIterator. Letztere darf nicht parametrisch sein.
 *    Vielen Dank an den aufmerksamen Hinweisgeber aus dem Publikum!
 *
 * @author Steffen Jost, Lehrstuhl TCS, LMU München
 * Created Di, 23.Januar 2018
 */

public class MyList3<E> implements MyListInterface<E> {

  private static class Link<E> {
    E       data;
    Link<E> next;
    Link<E> prev;

    private Link(Link<E> prev, E data, Link<E> next) {
      this.data = data;
      this.next = next;
      this.prev = prev;
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

    private Link<E> getPrev() {
      return prev;
    }

    private void setPrev(Link<E> prev) {
      this.prev = prev;
    }
  }

  // Instanzvariablen der Liste
  Link<E> first;
  Link<E> last;  // Klasseninvariante first==null impliziert last==null


  public MyList3() {
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
    if (newSecond != null) {
      newSecond.setPrev(first);
    } else {
      last = first;
    }
  }

  public void addLast(E e) {
    Link<E> altesLetzes = last;
    this.last = new Link<>(altesLetzes, e, null);
    if (altesLetzes != null) {
      altesLetzes.setNext(last);
    } else {
      first = last;
    }
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
    E kopfData = first.getData();
    Link<E> second = first.getNext();
    if (second!=null) {
      second.setPrev(null);
    } else { // Liste hatte nur ein Element gehabt. Jetzt ist es weg.
      last = null;
    }
    first = second;
    return kopfData;
  }

  /**
   * @return Iterator über die Liste
   */
  @Override
  public ListIterator<E> iterator() {
    return new MyIterator();
  }

  private class MyIterator implements ListIterator<E> {

    //MyList3<E> liste; ist jetzt implizit vorhanden als MyList3.this (im Gegensatz zur Instanzvariablen immer final)
    Link<E> positionsZeiger;
    Link<E> letztGelesenesPos;
    int index;

    private MyIterator() {
      this.positionsZeiger   = MyList3.this.first;
      this.letztGelesenesPos = null;
      this.index = 0;
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
      letztGelesenesPos = positionsZeiger;
      positionsZeiger   = positionsZeiger.getNext();
      index++;
      return letztGelesenesPos.getData();
    }

    //ENDE ÜBERARBEITUNG IN DER VORLESUNG AM 23.01.18 //

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
      if (positionsZeiger == null) { // Wir sind am Ende der Liste!
        return letztGelesenesPos != null;
      } else { // letztGelensesE hilft nicht, es könnte ein previous Aufruf erfolgt sein!
        return positionsZeiger.getPrev() != null;
      }
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
      if (positionsZeiger == null) { // Wir sind total am Ende!
        if (letztGelesenesPos == null) return null;
        else {
          positionsZeiger = letztGelesenesPos;
          index--;
          return letztGelesenesPos.getData();
        }
      } else {
        Link<E> pre = positionsZeiger.getPrev();
        index--;
        if (pre == null) return null;
        else return pre.getData();
      }
    }

    public E previous2() { // Hässliche Alternative, wenn man Exception-Handling üben will.
      try {
        if (positionsZeiger == null) { // Wir sind total am Ende!
          positionsZeiger = letztGelesenesPos;
          index--;
          return letztGelesenesPos.getData(); // NullPointerException hier möglich
        } else {
          Link<E> pre = positionsZeiger.getPrev(); // NullPointerException hier möglich
          positionsZeiger = pre;
          index--;
          return pre.getData();
        }
      } catch (NullPointerException e) {
          return null; //Gefährlich, falls es noch einen dritten Fall geben könnte, der NullPointerException auslöst, an den wir nicht gedacht haben!!!
      }
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
      return index;
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
      return index-1;
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
        if (letztGelesenesPos == null) throw new IllegalStateException();
        Link<E> nachfolger = letztGelesenesPos.getNext();
        Link<E> vorgaenger = letztGelesenesPos.getPrev();
        if (nachfolger != null) {
          nachfolger.setPrev(vorgaenger);
        } else {
          MyList3.this.last = vorgaenger;
        }
        if (vorgaenger != null) {
          vorgaenger.setNext(nachfolger);
        } else {
          MyList3.this.first = nachfolger;
        }
        letztGelesenesPos = null;
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
      if (letztGelesenesPos == null) throw new IllegalStateException();
      letztGelesenesPos.setData(e);
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
      if (positionsZeiger == null) { MyList3.this.addLast(e); }
      else {
        Link<E> vorgaenger = positionsZeiger.getPrev();
        Link<E> neuesGlied = new Link<>(vorgaenger, e, positionsZeiger);
        positionsZeiger.setPrev(neuesGlied);
        if (vorgaenger == null) {
          MyList3.this.first = neuesGlied;
        } else {
          vorgaenger.setNext(neuesGlied);
        }
      }
    }
  }

}
