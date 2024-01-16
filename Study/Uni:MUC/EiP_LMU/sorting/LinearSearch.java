public class LinearSearch
{  public static void main(String[] args)
   {  ConsoleReader console = new ConsoleReader(System.in);
   
      // construct random array
   
      int[] a = ArrayUtil.randomIntArray(20, 100);

      ArrayUtil.print(a);
      System.out.println("Enter number to search for:");
      int n = console.readInt();

      int j = search(a, n);
      System.out.println("Found in position " + j);
   }

   /**
      Finds a value in an array, using the linear search 
      algorithm.
      @param a the array 
      @param v the value to search
      @return the index at which the value occurs, or -1
      if it does not occur in the array
   */
   public static int search(int[] a, int v)
   {  for (int i = 0; i < a.length; i++)
      {  if (a[i] == v)
            return i;
      }
      return -1;
   }
}
