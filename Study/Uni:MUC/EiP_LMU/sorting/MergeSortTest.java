package sorting;
public class MergeSortTest
{  public static void main(String[] args)
   {  int[] a = ArrayUtil.randomIntArray(20, 100);
      ArrayUtil.print(a);
      MergeSort.sort(a);
      ArrayUtil.print(a);
      String input;
      boolean fertig = false;
      while(!fertig) {
	  input = JOptionPane.showInputDialog("Welche Zahl suchen Sie?");
	  fertig = input==null;
	  int x = Integer.parseInt(input);
	  boolean ergebnis = sucheVonBis(a,
      
   }
    public static boolean <T implements Comparable<T>>sucheVonBis(
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
