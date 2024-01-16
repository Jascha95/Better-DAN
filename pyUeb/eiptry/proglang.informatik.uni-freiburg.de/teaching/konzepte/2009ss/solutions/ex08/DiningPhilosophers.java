// Solution of the dining philosophers problem due to Tanenbaum
public class DiningPhilosophers {
    static int n = 5; // number of philosophers
    static Mutex mutex = new Mutex(true);
    static Mutex[] philMutex = new Mutex[n];
    static String[] status = new String[n];

    static void log(String s) {
        String t = System.currentTimeMillis() + "";
        System.out.println("[" +  t.substring(t.length() - 5)+ "] " + s);
    }
    
    // index of left neighbour
    static int left(int i) { 
        return (i + n - 1) % n;
    }
    
    // index of right neighbour
    static int right(int i) { 
        return (i + 1) % n;
    }
    
    // check whether philosopher i may eat
    static void test(int i) {
        if ("hungry".equals(status[i]) &&
            !"eating".equals(status[left(i)]) &&
            !"eating".equals(status[right(i)])) {
            status[i] = "eating";
            philMutex[i].mutexSignal();
        }
    }
    
    // start eating
    static void takeForks(int i) {
        mutex.mutexWait();
        status[i] = "hungry";
        test(i); 
        mutex.mutexSignal();
        philMutex[i].mutexWait(); // blocks if test(i) did not succeed;
    }
    
    // finish eating
    static void putForks(int i) {
        mutex.mutexWait();
        status[i] = "thinking";
        // check the neighbours
        test(left(i));
        test(right(i));
        mutex.mutexSignal();
    }
    
    static void think(int i) {
        try {
            Thread.sleep(200);
        } catch (InterruptedException e) {
        }
    }
    
    static void eat(int i) {
        log("philosopher " + i + " starts eating");
        try {
            Thread.sleep(5000);
        } catch (InterruptedException e) {
        } finally {
            log("philosopher " + i + " finished eating");
        }
    }
    
    public static void main(String[] args) {
        for (int i = 0; i < n; i++) {
            philMutex[i] = new Mutex(false);
            status[i] = "thinking";
        }
        final int nloops = 15;
        Runnable[] philosophers = new Runnable[n];
        for (int i = 0; i < n; i++) {
            final int j = i;
            Runnable phil = new Runnable() {
                @Override
                public void run() {
                    log("philosopher " + j + " now at the table");
                    int k = nloops;
                    while (k-- >= 0) {
                        think(j);
                        takeForks(j);
                        eat(j);
                        putForks(j);
                    }
                }            
            };
            philosophers[i] = phil;
        }
        for (Runnable r : philosophers) {
            new Thread(r).start();
        }
    }
}
