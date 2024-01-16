import java.util.Iterator;
import java.util.ListIterator;

/**
 * Demonstration der von selbst erstellten verketteten Listen.
 *
 * Siehe dazu auch Übungsaufgabe A12-1.
 *
 * @author Steffen Jost, Lehrstuhl TCS, LMU München
 */
public class MainDbl {


  /* ALTE AUSGABE:
Entfernt: Sigrid
slist1 hat Länge 2.
Uli, Martin,
ilist hat Länge 4.
3, 2, 1, 0,
[Max, Steffen, ], [Uli, Martin, ],
   */
  public static void main(String[] args) {
	  MyList2<String>  slist1 = new MyList2<>();
	  MyList2<String>  slist2 = new MyList2<>();
	  MyList2<Integer> ilist  = new MyList2<>();
    slist1.addFirst("Martin");
    slist2.addFirst("Steffen");
    ilist.addFirst(0);
    ilist.addFirst(1);
    slist1.addFirst("Sigrid");
    slist2.addFirst("Max");
    System.out.println("Entfernt: " + slist1.removeFirst());
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
    MyList2<MyList2<String>> slistlist = new MyList2<>();
    slistlist.addFirst(slist1);
    slistlist.addFirst(slist2);
    for(MyList2<String> l : slistlist) {
      System.out.print("[");
      Iterator<String> iter = l.iterator();
      while(iter.hasNext()){
        System.out.print(iter.next() + ", ");
      }
      System.out.print("], ");
    }
    slist1.addLast("Letztes");
    slist1.addFirst("Erstes");
    ListIterator<String> iter = slist1.iterator();
    iter.next();
    iter.next();
    iter.add("Mitte");
    iter.next();
    iter.remove();
    // Ausgabe 2
    System.out.println("ilist hat Länge "+slist1.size()+".");
    for(String i : slist1) {
      System.out.print(i + ", ");
    }

// WARNUNG NEVER EVER SOWAS -- BESSER MIT FOR-EACH:
//    for(int i = 0; i < slist1.size(); i++) {
//      System.out.print(slist1.get(i) + ", ");
//    }

  }

}

















