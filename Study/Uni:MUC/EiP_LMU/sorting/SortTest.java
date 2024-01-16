package sorting;
import javax.swing.JOptionPane;
public class SortTest
{  public static void main(String[] args)
    {        int n = Integer.parseInt(JOptionPane.showInputDialog("Enter array size:"));
      int[] a = ArrayUtil.randomIntArray(n, 100);
      StopWatch timer = new StopWatch();
      //ArrayUtil.print(a);
      timer.start();
      SelSort.sort(a);
      timer.stop();
      //    ArrayUtil.print(a);
	   System.out.println("Elapsed time: "
	  + timer.getElapsedTime() + " milliseconds");
   }
}

   
