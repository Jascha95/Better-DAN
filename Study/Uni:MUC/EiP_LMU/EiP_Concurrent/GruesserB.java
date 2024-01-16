import java.util.Date;
public class GruesserB extends Thread {
    final static int DELAY = 1000; 
    private String botschaft;
    
    public GruesserB(String s) {
        botschaft = s;
    }
    public void run() {
        try {
          while (true) {
              if (isInterrupted())
                  throw new InterruptedException();
	      Date jetzt = new Date();
	      System.out.println(jetzt + " " + botschaft);
	      sleep(DELAY);       // sleep wurde geerbt.
	  }
	}
	catch (InterruptedException e){
	    System.out.println("Fertig (" + botschaft + ")");
	}
    }

 

public static void main(String[] args) {
        GruesserB th1 = new GruesserB("Guten Tag.");
        GruesserB th2 = new GruesserB("Auf Wiedersehen.");
	WatchDog w = new WatchDog(th2);
        th1.start();
        th2.start();
	w.start();
        System.out.println("Grüßer gestartet!");
    }
}

class WatchDog extends Thread{
    private Thread t;
    WatchDog(Thread t) { this.t = t; }
    public void run() {
        try {
            sleep(10000);
            t.interrupt();
        }
        catch (InterruptedException e) {}
    }
}
