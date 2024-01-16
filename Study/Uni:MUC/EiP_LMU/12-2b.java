12-2b.java 

import java.util.LinkedList;
import java.util.ListIterator;





public class Test {
public static <E extends Comparable <? super E>> void einfuegen( E elem, LinkedList<E> list)
	{

	ListIterator <E> iter = list.ListIterator();
	boolean notfound = true;
	while(iter.hasNext() && notfound){
		notfoun = elem.comparTo(iter.next())>=0;

	}
	if (!notfound)
		{ iter.previous();
		}
		
		


	}
//c) 2 Klassen damit "einfuegen" nichtmehr kompiliert 

public class gehtnicht{

}

public class gibtsnicht{

}











       }