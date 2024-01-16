public class BinarySearch   
{  public static void main(String[] args)
   {  ConsoleReader console = new ConsoleReader(System.in);
   
      // construct random array and sort it
   
      int[] a = ArrayUtil.randomIntArray(20, 100);
      SelSort.sort(a);

      ArrayUtil.print(a);
      System.out.println("Enter number to search for:");
      int n = console.readInt();
      
      int j = search(a, n);
      
      System.out.println("Found in position " + j);
   }
   
   /**
      Finds a value in a range of a sorted array, 
      using the binary search algorithm.
      @param a the sorted array
      @param from the first index in the range to search
      @param to the last index in the range to search
      @param v the value to search
      @return the index at which the value occurs, or -1
      if it does not occur in the array
   */
   public static int binarySearch(int[] a, 
      int from, int to, int v)
   {  if (from > to)
         return -1;
         
      int mid = (from + to) / 2;
      int diff = a[mid] - v;
      
      if (diff == 0) // a[mid] == v
         return mid;
      else if (diff < 0) // a[mid] < v 
         return binarySearch(a, mid + 1, to, v);
      else
         return binarySearch(a, from, mid - 1, v);
   }
   
   /**
      Finds a value in a sorted array, using the binary
      search algorithm.
      @param a the sorted array
      @param v the value to search
      @return the index at which the value occurs, or -1
      if it does not occur in the array
   */
   public static int search(int[] a, int v)
   {  return binarySearch(a, 0, a.length - 1, v);
   }
}

