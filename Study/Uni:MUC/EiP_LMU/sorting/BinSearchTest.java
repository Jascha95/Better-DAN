package sorting;

import javax.swing.JOptionPane;
public class BinSearchTest
{  public static void main(String[] args)
   {  Integer[] a = ArrayUtil.randomIntegerArray(20, 100);
      ArrayUtil.print(a);
      MergeSort.sort(a);
      ArrayUtil.print(a);
      String input;
      boolean fertig = false;
      while(!fertig) {
	  input = JOptionPane.showInputDialog("Welche Zahl suchen Sie?");
	  fertig = input==null;
	  if (!fertig) {
	      Integer x = Integer.parseInt(input);
	      boolean ergebnis = sucheVonBis(a,x,0,a.length-1);
	      if (ergebnis) System.out.println("" + x + " ist vorhanden.");
	      else System.out.println("" + x + " ist nicht vorhanden.");
	  }
      }
   }


    public static <T extends Comparable<T>> boolean sucheVonBis(
            T[] l, T w, int i, int j)
    {
        if (i > j) return false;
        if (i == j) return 0 == l[i].compareTo(w);
        int m = (i+j) / 2;
        T wm = l[m];
        int comp = wm.compareTo(w);
        if (comp == 0) return true;
        if (comp < 0) // wm < w
            return sucheVonBis(l,w,m+1,j);
        else
            return sucheVonBis(l,w,i,m-1);
    }
      }
