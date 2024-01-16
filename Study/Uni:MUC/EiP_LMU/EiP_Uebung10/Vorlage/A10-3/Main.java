import java.util.LinkedList;
import java.util.List;

public class Main {

  public static void main(String[] args) throws InterruptedException {
    int numberOfRequests = Integer.parseInt(args[0]);
    List<Request> requests = new LinkedList<Request>();

    // alle requests erzeugen, und sie in der Reihenfolge
    // ihrer Erzeugung durchnummerieren 
    for (int i = 1; i <= numberOfRequests; i++)
      requests.add(new Request(Integer.toString(i)));

    // alle requests starten
    for (Request request : requests)
      request.start();

    // warten, bis alle requests beendet sind
    for (Request request : requests)
      request.join();

    // nach mehrfach vergebenen Nummern suchen
    boolean found = false;
    for (int i = 1; i <= numberOfRequests; i++) {
      List<Request> sameIdentifierRequests = new LinkedList<Request>();
      for (Request request : requests)
        if (request.getIdentifier() == i)
          sameIdentifierRequests.add(request);
      if (sameIdentifierRequests.size() > 1) {
        found = true;
        System.out.println("Requests with identifier " + i + ": "
            + sameIdentifierRequests);
      }
    }

    if (!found)
      System.out.println("All requests have unique identifiers");

    System.out.println("Sequence.currentValue(): " + Sequence.currentValue());
  }
}
