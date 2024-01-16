public class Train extends Thread {
   private final RailAB railAB;
   private final boolean west;

   public Train(final RailAB ab, final boolean w) {
      railAB = ab;
      west = w;
   }
   
   public void run() {
      if(west) {
         System.out.println("Try to go West!");
	 railAB.goWest();
      }
      else {
         System.out.println("Try to go East!");
         railAB.goEast();
      }
      railAB.leaveAB();
   }
}
