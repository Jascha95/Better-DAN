package hellointerface;

import java.rmi.*;

public interface HelloInterface extends Remote {
    String sayHello (String what)
	throws RemoteException;
}
