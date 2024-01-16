public class FibTrace
{  public static void main(String[] args)
   {  ConsoleReader console = new ConsoleReader(System.in);
      System.out.println("Enter n: ");
      int n = console.readInt();

      int f = fib(n);

      System.out.println("fib(" + n + ") = " + f);
   }

   /**
      Computes a Fibonacci number.
      @param n an integer
      @return the nth Fibonacci number
   */
   public static int fib(int n)
   {  System.out.println("Entering fib: n = " + n);
      int f;
      if (n <= 2) f = 1;
      else f = fib(n - 1) + fib(n - 2);
      System.out.println("Exiting fib: n = " + n
         + " return value = " + f);
      return f;
   }
}
