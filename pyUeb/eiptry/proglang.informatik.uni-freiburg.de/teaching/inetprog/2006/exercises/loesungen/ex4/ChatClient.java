package de.uni_freiburg.informatik.proglang.inetprog.ex4;

import java.rmi.Remote;
import java.rmi.RemoteException;

public interface ChatClient extends Remote {

    public void receiveMessage(String fromNickName, String msg) throws RemoteException;
    
}
