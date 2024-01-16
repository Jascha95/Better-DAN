import java.net.*;
import java.io.*;

public class TCPServer {
    ServerSocket ss;

    public TCPServer (int port)
	throws IOException {
	ss = new ServerSocket (port);
    }

    public void run (DialogHandler dh)
	throws IOException {
	boolean acceptingConnections = true;
	while (acceptingConnections) {
	    Socket s = ss.accept ();
	    BufferedReader br = new BufferedReader
		(new InputStreamReader (s.getInputStream ()));
	    PrintWriter pw = new PrintWriter (s.getOutputStream (), true);
	    acceptingConnections = dh.talk (br, pw);
	    s.close ();
	}
    }
}
