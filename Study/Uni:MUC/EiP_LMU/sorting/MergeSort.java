package sorting;
import java.util.ArrayList;
public class MergeSort
{  /**
      Merges two adjacent subranges of an array
      @param a the array with entries to be merged
      @param from the index of the first element of the 
         first range
      @param mid the index of the last element of the 
         first range
      @param to the index of the last element of the
         second range
   */
   public static void merge(int[] a, 
      int from, int mid, int to)
   {  int n = to - from + 1;
         // size of the range to be merged

      // merge both halves into a temporary array b 
      int[] b = new int[n];

      int i1 = from;
         // next element to consider in the first range
      int i2 = mid + 1;
         // next element to consider in the second range
      int j = 0; 
         // next open position in b

      // as long as neither i1 nor i2 past the end, move
      // the smaller element into b
      while (i1 <= mid && i2 <= to)
      {  if (a[i1] < a[i2])
         {  b[j] = a[i1];
            i1++;
         }
         else
         {  b[j] = a[i2];
            i2++;
         }
         j++;
      }
      // note that only one of the two while loops
      // below is executed

      // copy any remaining entries of the first half
      while (i1 <= mid)
      {  b[j] = a[i1];
         i1++;
         j++;
      }
      
      // copy any remaining entries of the second half
      while (i2 <= to)
      {  b[j] = a[i2];
         i2++;
         j++;
      }

      // copy back from the temporary array
      for (j = 0; j < n; j++)
         a[from + j] = b[j];
   }

   public static <T extends Comparable<? super T>>void merge(ArrayList<T> a, 
      int from, int mid, int to)
   {  int n = to - from + 1;
         // size of the range to be merged

      // merge both halves into a temporary array b 
       ArrayList<T> b = new ArrayList<T>(n);

      int i1 = from;
         // next element to consider in the first range
      int i2 = mid + 1;
         // next element to consider in the second range
      int j = 0; 
         // next open position in b

      // as long as neither i1 nor i2 past the end, move
      // the smaller element into b
      while (i1 <= mid && i2 <= to)
	  {  if (a.get(i1).compareTo(a.get(i2)) > 0)
	      {  b.set(j,a.get(i1));
            i1++;
         }
         else
	     {  b.set(j,a.get(i2));
		      i2++;
         }
         j++;
      }
      // note that only one of the two while loops
      // below is executed

      // copy any remaining entries of the first half
      while (i1 <= mid)
	  {  b.set(j,a.get(i1));
         i1++;
         j++;
      }
      
      // copy any remaining entries of the second half
      while (i2 <= to)
	  {  b.set(j,a.get(i2));
         i2++;
         j++;
      }

      // copy back from the temporary array
      for (j = 0; j < n; j++)
	  a.set(from + j,b.get(j));
   }


   /**
      Sorts a range of an array, using the merge sort
      algorithm.
      @param a the array to sort
      @param from the first index of the range to sort
      @param to the last index of the range to sort
   */
    public static void mergeSort(int[] a, int from, int to)
   {  
       if (from == to) return;
      int mid = (from + to) / 2;
       // sort the first and the second half
      mergeSort(a, from, mid);
      mergeSort(a, mid + 1, to);
      //      ArrayUtil.print(a);
      merge(a, from, mid, to);


   }
    public static <T extends Comparable<? super T>> void mergeSort(ArrayList<T> a, int from, int to)
   {  
       if (from == to) return;
      int mid = (from + to) / 2;
       // sort the first and the second half
      mergeSort(a, from, mid);
      mergeSort(a, mid + 1, to);
      //      ArrayUtil.print(a);
      merge(a, from, mid, to);


   }

   /**
      Sorts an array, using the merge sort algorithm.
      @param a the array to sort
   */
   public static <T extends Comparable<? super T>>void sort(ArrayList<T> a)
    {  mergeSort(a, 0, a.size() - 1);
   }
}
