public class TrainNet {
   public final static int MAX_THREADS = 10;
   public final static int MAX_TRAINS = 3;

   public static void main(String[] args) {
      RailAB ab= new RailAB(MAX_TRAINS);
      for(int i = 0; i < MAX_THREADS; i++) {
         Train train = new Train(ab, (i % 2 == 1));
         train.start();
      }
   }
}

