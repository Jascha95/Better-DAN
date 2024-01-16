import java.io.*;

public class BackTalkDialog
    implements DialogHandler {

    public boolean talk (BufferedReader br, PrintWriter pw) {
	String line = null;
	BufferedReader terminal = new BufferedReader
	    (new InputStreamReader (System.in));
	while (true) {
	    try {
		if (br.ready ()) {
		    line = br.readLine ();
		    System.out.println (line);
		} else if (terminal.ready ()) {
		    line = terminal.readLine ();
		    if (line.equals ("STOP!")) {
			break;
		    }
		    pw.println (line);
		}
	    } catch (IOException ioe) {
		return false;
	    }
	}
	return false;		// stop the server
    }
}

    
