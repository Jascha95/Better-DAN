package helloimplement;

import java.rmi.*;
import java.rmi.server.UnicastRemoteObject;
import java.net.*;

public class Hello
    extends UnicastRemoteObject
    implements hellointerface.HelloInterface {

    private String name;

    public Hello (String s) throws RemoteException {
	super ();
	name = s;
    }

    public String sayHello (String what) throws RemoteException {
	System.out.println ("I got " + what);
	return (name + " says: " + what);
    }

    public static void main (String args[]) {
	if (System.getSecurityManager () == null) {
	    System.out.println ("setting new SecurityManager");
	    System.setSecurityManager (new RMISecurityManager ());
	}
	try {
	    Hello obj = new Hello (args[0]);
	    String myHostName = InetAddress.getLocalHost().getHostName();
	    String myRegistryEntry = "//" + myHostName + "/HelloServer";
	    Naming.rebind (myRegistryEntry, obj);
	    System.out.println ("Registered as \"" + myRegistryEntry + "\"");
	} catch (Exception re) {
	    System.out.println ("Caught " + re);
	}
    }
}
