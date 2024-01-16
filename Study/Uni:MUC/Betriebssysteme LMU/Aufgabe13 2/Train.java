public class Train extends Thread {
   private RailAB  railAB;
   private boolean west;
   
   public Train(RailAB ab, boolean w) {
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
