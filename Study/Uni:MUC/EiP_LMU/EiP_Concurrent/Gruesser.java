import java.util.Date;

public class Gruesser implements Runnable{
  final static int DELAY = 1000;
  private String botschaft;

  public Gruesser(String botschaft) {
      this.botschaft = botschaft;
  }
  @Override
  public void run() {
      try {
          while (true) {
              Date jetzt = new Date();
              System.out.println(jetzt + " " + botschaft);
              Thread.sleep(DELAY);
          }         
      } catch (InterruptedException e) {}
  }
  public static void main(String[] args) {
      Gruesser g1 = new Gruesser("Guten Tag.");
      Thread th1 = new Thread(g1);
      Thread th2 = new Thread(new Gruesser("Auf Wiedersehen."));
      th1.start();
      th2.start();
      System.out.println("Grüßer gestartet!");
  }
}
