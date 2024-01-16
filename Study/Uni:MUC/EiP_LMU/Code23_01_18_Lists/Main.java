import java.util.Iterator;

/**
 * Demonstration der von selbst erstellten verketteten Listen.
 *
 * Siehe dazu auch Übungsaufgabe A12-1.
 *
 * @author Steffen Jost, Lehrstuhl TCS, LMU München
 */
public class Main {

  public static void main(String[] args) {
	  MyList<String>  slist1 = new MyList<>();
	  MyList<String>  slist2 = new MyList<>();
	  MyList<Integer> ilist  = new MyList<>();
    slist1.addFirst("Martin");
    slist2.addFirst("Steffen");
    ilist.addFirst(0);
    ilist.addFirst(1);
    slist1.addFirst("Sigrid");
    slist2.addFirst("Max");
    ilist.addFirst(2);
    ilist.addFirst(3);
    slist1.addFirst("Uli");
    // Ausgabe 1
    System.out.println("slist1 hat Länge "+slist1.size()+".");
    for(String name : slist1) {
      System.out.print(name+", ");
    }
    System.out.println("");
    // Ausgabe 2
    System.out.println("ilist hat Länge "+ilist.size()+".");
    for(Integer i : ilist) {
      System.out.print(i + ", ");
    }
    System.out.println("");
    // Verschachtelte Liste:
    MyList<MyList<String>> slistlist = new MyList<>();
    slistlist.addFirst(slist1);
    slistlist.addFirst(slist2);
    for(MyList<String> l : slistlist) {
      System.out.print("[");
      Iterator<String> iter = l.iterator();
      while(iter.hasNext()){
        System.out.print(iter.next() + ", ");
      }
      System.out.print("], ");
    }
  }

}














