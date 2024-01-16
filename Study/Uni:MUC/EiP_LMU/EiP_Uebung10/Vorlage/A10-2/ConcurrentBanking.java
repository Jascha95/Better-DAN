/**
 *  Variation des Beispiels von Folie 12.45
 */

public class ConcurrentBanking implements Runnable {
    final static int DELAY = 1000;

    private final BankAccount fromA;
    private final BankAccount fromB;
    private final BankAccount recipient;

    public ConcurrentBanking(BankAccount fromA, BankAccount fromB, BankAccount recipient) {
        this.fromA = fromA;
        this.fromB = fromB;
        this.recipient = recipient;
    }

    public void run() {
        System.out.println("Thread " + Thread.currentThread().getId() + " started, transfers to " + recipient.getId());
        try {
            while (true) {
                    fromA.transfer(500, recipient);
                    fromB.transfer(500, recipient);
            }
        } catch (InterruptedException e) {
        }
    }

    public static void main(String[] args) throws InterruptedException {
        BankAccount account0 = new BankAccount("A",1000);
        BankAccount account1 = new BankAccount("B",1000);
        BankAccount account2 = new BankAccount("C",1000);

        Thread th0 = new Thread(new ConcurrentBanking(account1,account2,account0), "100");
        Thread th1 = new Thread(new ConcurrentBanking(account0,account2,account1), "101");
        Thread th2 = new Thread(new ConcurrentBanking(account0,account1,account2), "102");
        th0.start();
        th1.start();
        th2.start();
        while (true)  {
            Thread.currentThread().sleep(DELAY);
            double b0 = account0.getBalance();
            double b1 = account1.getBalance();
            double b2 = account2.getBalance();
            System.out.println(account0.getId() + ":" + b0 + "; " +
                               account1.getId() + ":" + b1 + "; " +
                               account2.getId() + ":" + b2 + "; "
            );
        }

    }
}
