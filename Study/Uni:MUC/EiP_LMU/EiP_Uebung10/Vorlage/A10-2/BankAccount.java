/**
 * Synchronisiertes Bankkonto
 */
public class BankAccount {

    private double balance;
    private final String id;

    BankAccount(String id, int i) {
        this.id = id;
        balance = i;
    }

    public synchronized void deposit(double amount) {
        balance += amount;
        System.out.println("Thread " + Thread.currentThread().getId() + " depositing " + amount + " to " + getId() + ". New balance is " + balance);
        this.notifyAll();
    }

    public synchronized void withdraw(double amount) throws InterruptedException {
        while (balance < amount) {
            System.out.println("Thread " + Thread.currentThread().getId() + " withdrawing " + amount + " from " + getId() + ". Waiting with balance " + balance);
            this.wait();
        }
        balance -= amount;
        System.out.println("Thread " + Thread.currentThread().getId() + " withdrawing " + amount + " from " + getId() + ". New balance is " + balance);
    }

    public synchronized void transfer(double amount, BankAccount recipient) throws InterruptedException {
        synchronized (recipient) {
            this.withdraw(amount);
            recipient.deposit(amount);
        }
    }

    // Für Print-Debugging:
    public String getId() {
        return id;
    }

    public double getBalance() {
        return balance;
    }
}
