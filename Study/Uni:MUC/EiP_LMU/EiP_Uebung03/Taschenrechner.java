/*
 *  Einführung in die Programmierung
 *  LMU München, Lehrstuhl TCS
 *  Wintersemester 2017/18
 *
 *  Dateivorlage für Übung A3-1.
 */


import java.util.Scanner;

public class Taschenrechner {

  public static void main(String[] args) {
      Scanner scanner = new Scanner(System.in);
      String  helpmsg = "(Eingabe \"end\" zum Beenden)";
      
      System.out.print("Dies ist ein Taschenrechner. ");
      System.out.println(helpmsg);

      
      // TODO -- IHRE AUFGABE !!!


      // Vielleicht ein nützliche Code-Schnipsel? 
      while (!scanner.hasNextInt()) {
        String input = scanner.next();   
          // Falls nein, Eingabe aus Scanner herausnehmen.
        System.out.println("Das war keine ganze Zahl! ");
      }
      
      int input = scanner.nextInt();   // Falls ja, Int auslesen;
      


      System.out.println("Endstand ist: " + input);
      scanner.close();
  }

}
