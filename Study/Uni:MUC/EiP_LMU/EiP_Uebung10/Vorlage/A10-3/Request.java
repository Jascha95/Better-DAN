public class Request extends Thread {

  private Integer identifier;

  public Request(String number) {
    super(number);
    identifier = null;
  }

  public Integer getIdentifier() {
    return identifier;
  }

  @Override
  public String toString() {
    return "Request No " + getName();
  }

  @Override
  public void run() {
    identifier = Sequence.nextValue();
  }

}