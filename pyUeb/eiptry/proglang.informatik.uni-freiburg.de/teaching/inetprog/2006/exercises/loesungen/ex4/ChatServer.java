package de.uni_freiburg.informatik.proglang.inetprog.ex4;

import java.rmi.Remote;
import java.rmi.RemoteException;


public interface ChatServer extends Remote {
    
    public void register(String nickName, ChatClient client) throws RemoteException;
    public void distributeMessage(String nickName, String message)
        throws RemoteException;
    
}
