public class ArraySpiegeln {
  /*
   * Vorlage EiP-Übung, Lehrstuhl TCS, LMU München.
   */
  public static String arrayToStr(String[] input, String glue) {
    return "";
  }

  public static String[] arraySpiegel(String[] input) {
    String[] ergebnis = new String[0];
    // *** TODO *** 
    return ergebnis;
  }
    
  public static void inPlaceMirror(String[] input) {
    // *** TODO *** 
  }

  public static void main(String[] arg) {
    System.out.println("Die Parameter lauten: " + arrayToStr(arg , ", "));
    String[] arg2 = arraySpiegel(arg);
    System.out.println("... und gespiegelt:   " + arrayToStr(arg2, ", "));
    System.out.println("... und ursprünglich: " + arrayToStr(arg , ", "));
    inPlaceMirror(arg);
    System.out.println("... wieder gespiegelt:" + arrayToStr(arg , ", "));      
  }    
}
