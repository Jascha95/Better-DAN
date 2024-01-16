package sorting;
public class MergeSortTime
{  public static void main(String[] args)
   {  ConsoleReader console = new ConsoleReader(System.in);
   
      // construct random array
   
      System.out.println("Enter array size:");
      int n = console.readInt();
      int[] a = ArrayUtil.randomIntArray(n, 100);
      
      // use stopwatch to time selection sort

      StopWatch timer = new StopWatch();

      timer.start();
      MergeSort.sort(a);
      timer.stop();

      System.out.println("Elapsed time: " 
         + timer.getElapsedTime() + " milliseconds");
   }
}

   
