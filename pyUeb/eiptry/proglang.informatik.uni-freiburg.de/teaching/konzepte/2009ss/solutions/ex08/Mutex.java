
public class Mutex {

    private boolean isOpen;
    
    public Mutex(boolean isOpen) {
        this.isOpen = isOpen;
    }
    
    public synchronized void mutexWait() {
        while (! isOpen) {
            try {
                wait();
            } catch (InterruptedException e) {
                // do nothing
            }
        }
        isOpen = false;
        return;
    }
    
    public synchronized void  mutexSignal() {
        isOpen = true;
        notify();
    }
}
