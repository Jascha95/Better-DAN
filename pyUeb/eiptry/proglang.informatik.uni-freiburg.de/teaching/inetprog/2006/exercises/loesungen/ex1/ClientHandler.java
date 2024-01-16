package de.uni_freiburg.informatik.proglang.inetprog.ex1;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.Socket;

public class ClientHandler implements Runnable {

    private Socket m_sock;
    private SMTPServer m_smtpSrv;
    
    public ClientHandler(SMTPServer smtpSrv, Socket sock) {
        m_smtpSrv = smtpSrv;
        m_sock = sock;
    }
    
    public void run() {
        System.out.println("Talking with " + m_sock.getInetAddress() + " in thread " + Thread.currentThread());
        try {
            internRun();
        } catch(Exception e) {
            System.err.println("Exception occurred while talking with " + m_sock.getInetAddress());
            e.printStackTrace();
        }
    }
        
    private void internRun() throws Exception {
        PrintWriter out = new PrintWriter(m_sock.getOutputStream());
        BufferedReader in = new BufferedReader(new InputStreamReader(m_sock.getInputStream()));
        toClient(out, "Do you want to send an email?");
        String answ = fromClient(in);
        while ("yes".equals(answ)) {
            toClient(out, "Give me your email address!");
            String from = fromClient(in);
            toClient(out, "Give me the recipient's email address!");
            String to = fromClient(in);
            toClient(out, "What's the subject?");
            String subject = fromClient(in);
            toClient(out, "Enter the text of the email (only one line allowed)!");
            String text = fromClient(in);
            try {
                m_smtpSrv.sendMail(from, to, subject, text);
                toClient(out, "Email sent successfully!");
            } catch(Exception e) {
                toClient(out, "Sending email failed: " + e.getMessage());
            }
            toClient(out, "Do you want to send another email?");
            answ = fromClient(in);
        }
        toClient(out, "bye!");
        m_sock.close();
    }
    
    private String fromClient(BufferedReader in) throws Exception {
        String s = in.readLine();
        System.out.println("  => FROM CLIENT " + m_sock.getInetAddress() + ", thread " + Thread.currentThread() +
                          ": " + s);
        return s;
    }
    
    private void toClient(PrintWriter out, String s) throws Exception {
        System.out.println("  => TO CLIENT " + m_sock.getInetAddress() + ", thread " + Thread.currentThread() +
                           ": " + s);
        out.println(s);
        out.flush();
    }

}
