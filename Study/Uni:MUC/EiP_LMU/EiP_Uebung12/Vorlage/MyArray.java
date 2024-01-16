import java.lang.reflect.Array;
import java.util.NoSuchElementException;

/**
 * Implementierung des SimpleList Interface
 * mit Hilfe von klassischen Arrays zur Demonstration.
 *
 * Vorlesung "Einführung in die Programmierung", W17, LMU München
 *
 * @author jost
 * Created Fr, 19.Januar 2018
 */

public class MyArray<E> implements SimpleList<E> {
  E[] array;
  int size;

  public MyArray() {
    this.size = 0;
    // this.array = new E[2]; Leider nicht erlaubt in Java: Generics und Arrays vertragen sich nicht!
    this.array = (E[]) new Object[2]; // Typecast muss gut durchdacht sein!
  }

  @Override
  public boolean isEmpty() {
    return size == 0;
  }

  @Override
  public int size() {
    return this.size;
  }

  @Override
  public E get(int index) {
    if (index < size)
      return this.array[index];
    else
      throw new IndexOutOfBoundsException();
  }

  @Override
  public E set(int index, E element) {
    if (index < size) {
      E old = this.array[index];
      this.array[index] = element;
      return old;
    } else {
      throw new IndexOutOfBoundsException();
    }
  }

  @Override
  public void addFirst(E element) {
    E[] oldAry = this.array;
    if (size + 1 > this.array.length)
      this.array = (E[]) new Object[2 * oldAry.length]; // Typecast muss durchdacht sein!
    E current = element;
    for (int i = 0; i < size; i++) {
      E old = oldAry[i];
      this.array[i] = current;
      current = old;
    }
    this.array[size] = current;
    this.size = size + 1;
  }

  @Override
  public E removeFirst() {
    if (size < 1)
      throw new NoSuchElementException();
    E res = this.array[0];
    for (int i = 1; i < size; i++) {
      this.array[i - 1] = this.array[i];
    }
    this.size--;
    this.array[size] = null; // nicht notwendig, verhindert aber memory leak
    return res;
  }

  @Override
  public SimpleIterator<E> iterator() {
    return new MyArrayIterator<>(this);
  }

  /*
   * Private innere Klasse zur Implementierung des Iterators.
   */
  private class MyArrayIterator<E> implements SimpleIterator {
    int position;
    MyArray<E> myArray;

    MyArrayIterator(MyArray<E> myArray) {
      this.myArray = myArray;
      this.position = 0;
    }

    @Override
    public boolean hasNext() {
      return position < myArray.size();
    }

    @Override
    public Object next() {
      return myArray.get(position++);
    }
  }
}
