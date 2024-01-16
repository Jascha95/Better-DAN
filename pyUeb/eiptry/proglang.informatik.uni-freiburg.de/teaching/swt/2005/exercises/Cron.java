import java.util.Iterator;
import java.util.ArrayList;

abstract class Subject {
    
    private ArrayList observers = new ArrayList();
    
    public void attach(Observer o) {
	observers.add(o);
    }
    public void dettach(Observer o) { 
	observers.remove(o);
    }
    // notify is a reserved name in Java
    public void announce() {
	Iterator i = observers.iterator();
	while (i.hasNext()) { 
	    ((Observer)(i.next())).update(this);
	}
    }
}

class Clock extends Subject {

    private long seconds;
    
    public long getTime() { 
	return seconds;
    }
    public void setTime(long s) { 
	seconds = s;
	announce();
    }
    public void tick() { 
	seconds++;
	announce();
    } 
}

abstract class Observer {
    public abstract void update(Subject s);
}

class Cronjob extends Observer {
    String name = "";
    
    public Cronjob (String s) {
	name = s;
    }
    public void update(Subject s) { 
	System.out.println(name + ": " + 
			   ((Clock)(s)).getTime() + 
			   " seconds");
    }
}

public class Cron {

    private void run() {
	Clock clock = new Clock();
	Cronjob c1  = new Cronjob("c1");
	Cronjob c2  = new Cronjob("c2");
	clock.attach(c1);
	clock.attach(c2);
	clock.setTime(100);
	clock.tick();
	clock.dettach(c1);
	clock.tick();
    }


    public static void main(String args[]) {
	Cron c = new Cron();
	c.run();
    }
}
