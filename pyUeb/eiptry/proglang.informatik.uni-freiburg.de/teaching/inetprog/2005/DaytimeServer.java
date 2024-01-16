import java.io.*;
import java.net.*;
import java.util.*;

public class DaytimeServer {

    static final int BUFSIZE = 128;
    static final int DAYTIME = 13;	// portnumber for daytime service, RFC867

    public static void main (String[] arg) {
	if (arg.length > 1) {
	    System.out.println ("Usage: java DaytimeServer [port]");
	} else {
	    int port = DAYTIME;
	    if (arg.length == 1) {
		try {
		    port = new Integer (arg[0]).intValue ();
		} catch (Exception e) {
		    System.out.println ("Bad port number");
		}
	    }
	    try {
		serveTime (port);
	    } catch (Exception e) {
		System.out.println ("An error has occurred");
	    }
	}
    }

    public static void serveTime (int port)
	throws Exception {
	byte[] buffer = new byte[BUFSIZE];
	DatagramPacket p = new DatagramPacket (buffer, BUFSIZE);
	DatagramSocket s = new DatagramSocket (port);
	s.receive (p);		// contents do not matter
	Date d = new GregorianCalendar ().getTime ();
	System.out.println ("Sending: " + d);
	String answer = d.toString ();
	p.setData ((answer + "\r\n").getBytes ());
	p.setLength (answer.length () + 2);
	s.send (p);
	s.close ();
    }
}
