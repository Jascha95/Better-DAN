import java.util.NoSuchElementException;

/**
 * Implementierung des SimpleList Interface
 * mit Hilfe einer einfachen verketteten Liste.
 * Weitere Dokumentation siehe Interface!
 *
 * Kapitel 14
 * Vorlesung "Einführung in die Programmierung", W17, LMU München
 *
 * @author jost
 * Created Fr, 19.Januar 2018
 */

public class MyList<E> implements SimpleList<E> {

  /* Private innere Klasse zur welche
   * das Rückgrat (engl. "spine") einer
   * Liste repräsentiert.
   */
  private static class Link<E> {
    private E data;       // Verweis auf gespeichertes Datum
    private Link<E> next; // Verweis auf nächstes Listen-Glied

    Link(E data, Link<E> next) {
      this.data = data;
      this.next = next;
    }
    /* Bei einer inneren privaten Klasse erlauben wir uns
     * ausnahmsweise, direkt auf deren Instanzvariablen
     * zuzugreifen.
     * Der Einsatz von Getter/Setter-Methoden wäre
     * wartungsfreundlicher, liest sich aber umständlicher.
     */
  }

  /* Private innere Klasse, welches das
   * SimpleIterator-Interface implementiert.
   */
  private class MyListIterator<E> implements SimpleIterator<E> {
    private Link<E> position; // Position des Iterators

    MyListIterator(Link<E> position) {
      this.position = position;
    }

    @Override
    public boolean hasNext() {
      return position != null;
    }

    @Override
    public E next() {
      if (position==null)
        throw new NoSuchElementException();
      E res = position.data;
      position = position.next;
      return res;
    }
  }

  // Instanzvariablen:
  private Link<E> first; // Das erste Glied der Kette.

  // Konstrukor einer leeren Liste:
  public MyList() {
    this.first = null;
  }

  @Override
  public boolean isEmpty() {
    return first == null;
  }

  @Override
  public int size() {
    // Die Liste könnte die Größe auch in einer
    // Instanzvariable abspeichern, so wie bei MyArray
    int i = 0;
    SimpleIterator<E> iter = this.iterator();
    while (iter.hasNext()) {
      iter.next();
      i++;
    }
    return i;
  }

  @Override
  public E get(int index) {
    int i = 0;
    SimpleIterator<E> iter = this.iterator();
    while (i<index && iter.hasNext()) {
      iter.next();
      i++;
    }
    if (i != index)
      throw new IndexOutOfBoundsException();
    return iter.next();
  }

  @Override
  public E set(int index, E element) {
    int i = 0;
    Link<E> pos = this.first;
    while (i<index && pos != null) {
      pos = pos.next;
      i ++;
    }
    if (i != index)
      throw new IndexOutOfBoundsException();
    E res = pos.data;
    pos.data = element;
    return res;
  }

  @Override
  public void addFirst(E element) {
    Link<E> newLink = new Link<>(element, this.first);
    this.first = newLink;
  }

  @Override
  public E removeFirst() {
    if (first==null)
      throw new NoSuchElementException();
    E res = first.data;
    first = first.next;
    return res;
  }

  @Override
  public SimpleIterator<E> iterator() {
    return new MyListIterator<>(this.first);
  }
/*  public E previous    {                               //Vorlesung zuschrieb 25.01
      if  (position.Zeiger == null) {return null;
      if (letztelesePos = null) {return null;}
            }else {return letztelesePos.getData();
}
        Link<E> prev = positionZeiger.getData();
            if (prev == null) {return null; 
             } else {return prev.getData();}

            
                    }*/

  public void reverse (){ //Tutorium 31.01 swap->
    Link<E> temp;
    temp = first;
    first = last;
    last = temp;
    temp = first;
    Link<E> temp2;
    for (int i = 0; i < this.size(); i++){
        temp2 = temp.next;
        temp.next = temp.prev;
        temp.prev = temp2;
        temp = temp2;

    }
  }



}
