package sorting;

import javax.swing.JOptionPane;

public class SelSortTime
{  public static void main(String[] args)
   { 
   
      // construct random array
    int n = Integer.parseInt(JOptionPane.showInputDialog
			     ("Enter array size:"));
      int[] a = ArrayUtil.randomIntArray(n, 100);
      
      // use stopwatch to time selection sort
      
      StopWatch timer = new StopWatch();
      
      timer.start();
      SelSort.sort(a);
      timer.stop();
      
      System.out.println("Elapsed time: " 
         + timer.getElapsedTime() + " milliseconds");
   }
}

   
