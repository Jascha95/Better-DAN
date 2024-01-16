package de.uni_freiburg.informatik.proglang.inetprog.ex4;

import java.awt.BorderLayout;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.rmi.Naming;
import java.rmi.RMISecurityManager;
import java.rmi.RemoteException;
import java.rmi.server.UnicastRemoteObject;

import javax.swing.JFrame;
import javax.swing.JPanel;
import javax.swing.JTextArea;
import javax.swing.JTextField;

public class ChatClientImplementation extends UnicastRemoteObject
    implements ChatClient, ActionListener {

    private JTextArea m_messageArea;
    private JTextField m_inputField;
    private String m_nickName;
    private ChatServer m_server;
    
    public ChatClientImplementation(String nickName, ChatServer server) throws RemoteException {
        super();
        m_nickName = nickName;
        m_server = server;
    }
    
    private void initGUI() {
        JFrame.setDefaultLookAndFeelDecorated(true);
        //Create and set up the window.
        JFrame frame = new JFrame("Chat Client");
        frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);

        //Create and set up the panel.
        JPanel mainPanel = new JPanel(new BorderLayout());
        m_messageArea = new JTextArea(25,80);
        m_messageArea.setEditable(false);
        m_inputField = new JTextField();
        m_inputField.addActionListener(this);
        mainPanel.add(m_messageArea, BorderLayout.CENTER);
        mainPanel.add(m_inputField, BorderLayout.SOUTH);

        //Add the panel to the window.
        frame.getContentPane().add(mainPanel, BorderLayout.CENTER);

        //Display the window.
        frame.pack();
        frame.setVisible(true);
    }
    
    public void receiveMessage(final String fromNickName, final String msg) throws RemoteException {
        javax.swing.SwingUtilities.invokeLater(new Runnable() {
            public void run() {
                m_messageArea.append("\n[" + fromNickName + "] " + msg);
            }
        });        
    }

    public void actionPerformed(ActionEvent evt) {
        final String msg = m_inputField.getText();
        m_inputField.setText("");     
        final SwingWorker worker = new SwingWorker() {
            public Object construct() {
                try {
                    m_server.distributeMessage(m_nickName, msg);
                } catch (Exception e) {
                    e.printStackTrace();
                }
                return null;
            }
        };
        worker.start();
    }

    public static void main(String[] args) {
        if (args.length != 2) {
            System.out.println("Arguments: SERVER_HOST NICK_NAME");
            System.exit(1);
        }
        System.setProperty("java.rmi.server.ignoreStubClasses", "true");
        System.setProperty("java.security.policy", "chat_policy");
        System.setSecurityManager(new RMISecurityManager());
        String url = "//" + args[0] + "/ChatServer";
        String nickName = args[1];
        ChatServer server = null;
        try {
            server = (ChatServer) Naming.lookup(url);
        } catch (Exception e) {
            e.printStackTrace();
            System.exit(1);
        }
        try {
            final ChatClientImplementation chatClient = new ChatClientImplementation(nickName, server);
            server.register(nickName, chatClient);
            //Schedule a job for the event-dispatching thread:
            //creating and showing this application's GUI.
            javax.swing.SwingUtilities.invokeLater(new Runnable() {
                public void run() {
                    chatClient.initGUI();
                }
            });
        } catch (RemoteException e) {
            e.printStackTrace();
            System.exit(1);
        }
    }
}
