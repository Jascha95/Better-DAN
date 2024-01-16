import java.net.*;
import java.io.*;

public class BackTalk {

    public static void main (String[] arg) throws Exception {
	if (arg.length != 1) {
	    System.out.println ("Usage: BackTalk port");
	} else {
	    try {
		int port = new Integer (arg[0]).intValue ();
		TCPServer server = new TCPServer (port);
		server.run (new BackTalkDialog ());
	    } catch (RuntimeException e) {
		System.out.println ("Argument not an integer");
	    }
	}
    }
}
