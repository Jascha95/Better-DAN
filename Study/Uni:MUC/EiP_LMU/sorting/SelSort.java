package sorting;
public class SelSort
{  /**
      Finds the smallest element in an array range.
      @param a the array to search
      @param from the first position in a to compare
      @return the position of the smallest element in the
      range a[from]...a[a.length - 1]
   */
   public static int minimumPosition(Comparable[] a, int from)
   {  int minPos = from;
      for (int i = from + 1; i < a.length; i++)
	  if (a[i].compareTo(a[minPos]) < 0) minPos = i;
      return minPos;
   }

   /**
      Sorts an array.
      @param a the array to sort
   */
   public static void sort(Comparable[] a)
    { 	for (int n = 0; n < a.length - 1; n++)
	    {  int minPos = minimumPosition(a, n);
		//ArrayUtil.print(a);
		if (minPos != n)
		    ArrayUtil.swap(a, minPos, n);
      }
   }
}
