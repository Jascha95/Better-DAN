package helloclient;

import java.rmi.*;

public class HelloClient {

    public static void main (String args[]) {
	String message = ""; 
	System.setSecurityManager (new RMISecurityManager ());
	try {
	    String hostname = args[0];
	    hellointerface.HelloInterface hello =
		(hellointerface.HelloInterface)
		Naming.lookup ("//" + hostname + "/HelloServer");
	    for (int i=1; i<args.length; i++) {
		message = hello.sayHello (args[i]);
		System.out.println (message);
	    }
	} catch (Exception e) {
	    e.printStackTrace ();
	}
	return;
    }
    
}