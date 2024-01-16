public class Sequence {

  private static volatile int value = 0;

  /**
   * 
   * @return den nächsten Wert der Sequence
   */
  public static int nextValue() {
    return ++value;
  }

  /**
   * 
   * @return den aktuellen Wert der Sequence
   */
  public static int currentValue() {
    return value;
  }

}