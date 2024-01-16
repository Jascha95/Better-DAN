public class Benutzer implements Runnable {
  private CoffeeMachine machine;
  private int nummer;

  public Benutzer(CoffeeMachine machine, int nummer){
    this.machine = machine;
    this.nummer   = nummer;
  }

  public void run() {
    while (true) {
      machine.makeCoffee(this.nummer);
      machine.takeCoffee(this.nummer);
    }
  }
}
