package de.uni_freiburg.informatik.proglang.inetprog.ex4;

import java.rmi.Naming;
import java.rmi.RMISecurityManager;
import java.rmi.RemoteException;
import java.rmi.registry.LocateRegistry;
import java.rmi.registry.Registry;
import java.rmi.server.UnicastRemoteObject;
import java.util.HashMap;
import java.util.Map;

public class ChatServerImplementation extends UnicastRemoteObject implements
        ChatServer {

    private static void log(String msg) {
        System.out.println(msg);
    }
    
    private Map<String,ChatClient> m_clients = new HashMap<String,ChatClient>();
    
    public void register(String nickName, ChatClient client) throws RemoteException {
        if (m_clients.containsKey(nickName)) {
            throw new RemoteException("Nick name " + nickName + " already assigned!");
        }
        m_clients.put(nickName, client);
        log(nickName + " registered");
    }
    
    public void distributeMessage(String nickName, String message)
        throws RemoteException {
        log("distributing message '" + message + "' from " + nickName);
        for (Map.Entry<String,ChatClient> e : m_clients.entrySet()) {
            try {
                e.getValue().receiveMessage(nickName, message);
                log("Message sent to " + e.getKey());
            } catch (Exception exc) {
                exc.printStackTrace();
            }
        }
        log("distributing message finished");
    }
    
    public ChatServerImplementation() throws RemoteException {
        super();
    }

    public static void main(String[] args) throws Exception {
        if (args.length != 0) {
            System.err.println("no arguments expected!");
            System.exit(1);
        }
        System.setSecurityManager(new RMISecurityManager());
        log("starting chat server ...");
        Naming.rebind("ChatServer", new ChatServerImplementation());
    }
}
