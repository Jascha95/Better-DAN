class HTTPGet {
    public static void main (String[] args)
	throws Exception {
	if (args.length != 2) {
	    System.out.println ("Usage: java HTTPGet host path");
	} else {
	    String hostname = args[0];
	    String path = args[1];
	    TCPConnection tc = new TCPConnection (hostname, 80);
	    // send request
	    tc.println ("GET "+path+" HTTP/1.1");
	    tc.println ("Host: "+hostname);
	    tc.println ("");
	    // read & echo response
	    System.out.println ("--------------------------------------");
	    String line = tc.readLine ();
	    while (line != null) {
		System.out.println (line);
		line = tc.readLine ();
	    }
	    // may hang for a while
	    System.out.println ("--------------------------------------");
	}
    }
}
