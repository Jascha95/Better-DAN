import java.io.*;
import java.net.*;

public class Daytime {

    static final int BUFSIZE = 128;
    static final int DAYTIME = 13;	// portnumber for daytime service, RFC867

    public static void main (String[] arg) {
	if (arg.length != 1 && arg.length != 2) {
	    System.out.println ("Usage: java Daytime host [port]");
	} else {
	    String hostname = arg[0];
	    int port = DAYTIME;
	    try {
		if (arg.length == 2) {
		    port = new Integer (arg[1]).intValue ();
		}
	    } catch (Exception e) {
		System.out.println ("Illegal port number");
	    }
	    try {
		System.out.println
		    ("Time at " + hostname + " is \"" +
		     getTime (hostname, port) + "\"");
	    } catch (Exception e) {
		System.out.println ("Cannot get daytime from " + hostname);
	    }
	}
    }

    public static String getTime (String hostname, int port)
	throws Exception {
	byte[] buffer = new byte[BUFSIZE];
	InetAddress server = InetAddress.getByName (hostname);
	DatagramPacket answer = new DatagramPacket (buffer, BUFSIZE);
	DatagramSocket s = new DatagramSocket ();
	answer.setAddress (server);
	answer.setPort (port);
	s.send (answer);	// contents do not matter
	s.receive (answer);
	s.close ();
	int len = answer.getLength ();
	buffer = answer.getData ();
	while (buffer[len-1] == 10 || buffer[len-1] == 13) {
	    len--;
	}
	return new String (buffer, 0, len);
    }
}
