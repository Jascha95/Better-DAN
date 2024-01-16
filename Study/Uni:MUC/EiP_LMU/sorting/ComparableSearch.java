import java.util.ArrayList;
import java.util.Collections;

public class ComparableSearch
{  public static void main(String[] args)
   {    
       ArrayList<String> a = new ArrayList<String>();
       Collections.sort(a);
      String[] staff = new String[5];
      staff[0] = "Dick";
      staff[1] = "Harry";
      staff[2] = "Juliet";
      staff[3] = "Romeo";
      staff[4] = "Tom";
      
      for (int i = 0; i < staff.length; i++)
         System.out.println(staff[i]);
         
      System.out.println("Enter name to search for:");
      String name = javax.swing.JOptionPane.showInputDialog("");

      int j = search(staff, name);

      System.out.println("Found in position " + j);
   }

   /**
      Finds a value in a range of a sorted array, using the 
      binary search algorithm. The array objects must 
      implement the Comparable interface.
      @param a the sorted array
      @param from the first index in the range to search
      @param to the last index in the range to search
      @param v the object to search
      @return the index at which the object occurs, or -1
      if it does not occur in the array
   */
   public static <T> int binarySearch(Comparable<? super T>[] a, 
      int from, int to, T v)
   {  if (from > to)
         return -1;
      int mid = (from + to) / 2;
      int diff = a[mid].compareTo(v);
      if (diff == 0) // a[mid] == v
         return mid;
      else if (diff < 0) // a[mid] < v 
         return binarySearch(a, mid + 1, to, v);
      else
         return binarySearch(a, from, mid - 1, v);
   }
   
   /**
      Finds a value in a sorted array, using the 
      binary search algorithm. The array objects must 
      implement the Comparable interface.
      @param a the sorted array
      @param v the object to search
      @return the index at which the object occurs, or -1
      if it does not occur in the array
   */
   public static <T>int search(Comparable<T>[] a, T v)
   {  return binarySearch(a, 0, a.length - 1, v);
   }
}

