public class FibLoop
{  public static void main(String[] args)
   {  ConsoleReader console = new ConsoleReader(System.in);
      System.out.println("Enter n: ");
      int n = console.readInt();

      // use stopwatch to time Fibonacci number computation

      StopWatch timer = new StopWatch();

      timer.start();
      int f = fib(n);
      timer.stop();

      System.out.println("fib(" + n + ") = " + f);
      System.out.println("Elapsed time = "
         + timer.getElapsedTime() + " milliseconds");
   }

   /**
      Computes a Fibonacci number.
      @param n an integer
      @return the nth Fibonacci number
   */
   public static int fib(int n)
   {  if (n <= 2) return 1;
      int fold = 1;
      int fold2 = 1;
      int fnew = 1;
      for (int i = 3; i <= n; i++)
      {  fnew = fold + fold2;
         fold2 = fold;
         fold = fnew;
      }
      return fnew;
   }
}
