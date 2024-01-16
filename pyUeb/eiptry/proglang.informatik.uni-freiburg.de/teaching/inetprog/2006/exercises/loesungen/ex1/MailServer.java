package de.uni_freiburg.informatik.proglang.inetprog.ex1;

import java.net.InetAddress;
import java.net.ServerSocket;
import java.net.Socket;

public class MailServer {

    private int m_port;
    private SMTPServer m_smtpSrv;
    
    public MailServer(SMTPServer smtpSrv, int port) {
        m_smtpSrv = smtpSrv;
        m_port = port;
    }

    public void listen() throws Exception {
        ServerSocket ssock = new ServerSocket(m_port);
        while (true) {
            System.out.println("Waiting for connections...");
            Socket sock = ssock.accept();
            System.out.println("Connection established from " + sock.getInetAddress());
            ClientHandler handler = new ClientHandler(m_smtpSrv, sock);
            new Thread(handler).start();
        }
    }
    
    public static void main(String[] args) {
        if (args.length != 3) {
            System.err.println("Illegal commandline arguments, expected: SMTP-HOST SMTP-PORT PORT");
            System.exit(1);
        }
        try {
            InetAddress smtpAddr = InetAddress.getByName(args[0]); 
            int smtpPort = new Integer(args[1]);
            int port = new Integer(args[2]);
            SMTPServer smtp = new SMTPServer(smtpAddr, smtpPort);
            MailServer server = new MailServer(smtp, port);
            server.listen();
        } catch(Exception e) {
            System.err.println(e.getMessage());
            System.exit(1);
        }
    }
}
