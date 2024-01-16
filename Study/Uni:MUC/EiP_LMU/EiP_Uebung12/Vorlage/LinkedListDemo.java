/**
 * Einfache Demonstration des Unterschiedes
 * zwischen Arrays und verketteten Listen.
 *
 *
 * Vorlesung "Einführung in die Programmierung", W17, LMU München
 *
 * @author jost
 * Created Fr, 19.Januar 2018
 */

public class LinkedListDemo {
    
   public static void main(String[] args) {
	      SimpleList<Integer> l1 = new MyArray<>();
	      work(l1);
	      // TODO Speicherbild 1 hier!!!
	      SimpleList<Integer> l2 = new MyList<>();
          work(l2);
          // TODO Speicherbild 2 hier!!!
    }

    private static void work(SimpleList<Integer> list) {
        list.addFirst(4);
        list.addFirst(0);
        list.removeFirst();
        list.addFirst(2);
        list.addFirst(1);
        list.addFirst(1); 
        list.set(2,1+list.get(1));
        System.out.print("List length="+list.size()+": ");
        // Iterieren mit dem Iterator:
        SimpleIterator<Integer> iter = list.iterator();
        while (iter.hasNext()) {
            System.out.print(iter.next()+",");
        }
        // Äquivalent auch mit "for-each"-Schleife:
        // for (Integer i : list) { System.out.print(i+","); }
        System.out.println("");
        System.out.println("Yarak");
    }
}
