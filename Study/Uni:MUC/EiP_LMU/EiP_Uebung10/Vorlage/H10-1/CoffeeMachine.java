public class CoffeeMachine {
  private boolean working;

  public CoffeeMachine() {
    this.working = false;
  }

  public void makeCoffee(int nummer) {
    while (working) {
      // warten
    }
    System.out.println("Benutzer " + nummer + " bestellt Kaffee.");
    working = true;
  }

  public void takeCoffee(int nummer) {
    System.out.println("Benutzer " + nummer + " entnimmt Tasse.");
    working = false;
  }


  public static void main(String[] args) {
    CoffeeMachine machine = new CoffeeMachine();
    new Thread(new Benutzer(machine, 1)).start();
    new Thread(new Benutzer(machine, 2)).start();
  }
}
