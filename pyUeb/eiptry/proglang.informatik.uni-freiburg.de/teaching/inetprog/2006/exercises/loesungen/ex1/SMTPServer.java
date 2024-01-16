package de.uni_freiburg.informatik.proglang.inetprog.ex1;

import java.io.BufferedReader;
import java.io.InputStreamReader;
import java.io.PrintWriter;
import java.net.InetAddress;
import java.net.Socket;

public class SMTPServer {

    private static final int SERVICE_READY_CODE = 220;
    private static final int MAIL_ACTION_OK_CODE = 250;
    private static final int START_MAIL_INPUT_CODE = 354;
    private static final int SERVICE_CLOSING_CODE = 221;
    
    private InetAddress m_smtpServerAddr;
    private int m_port;
    
    public SMTPServer(InetAddress smtpServerAddr, int port) {
        m_smtpServerAddr = smtpServerAddr;
        m_port = port;
    }
    
    public void sendMail(String from, String to, String subject, String text) throws Exception {
        Socket sock = new Socket(m_smtpServerAddr, m_port);
        PrintWriter out = new PrintWriter(sock.getOutputStream());
        BufferedReader in = new BufferedReader(new InputStreamReader(sock.getInputStream()));
        InetAddress myAddr = InetAddress.getLocalHost();
        
        fromServer(in, SERVICE_READY_CODE);
        toServer(out, "HELO " + myAddr.getHostName());
        fromServer(in, MAIL_ACTION_OK_CODE);
        toServer(out, "MAIL FROM: " + from);
        fromServer(in, MAIL_ACTION_OK_CODE);
        toServer(out, "RCPT TO: " + to);
        fromServer(in, MAIL_ACTION_OK_CODE);
        toServer(out, "DATA");
        fromServer(in, START_MAIL_INPUT_CODE);
        toServer(out, "To: " + to);
        toServer(out, "From: " + from);
        toServer(out, "Subject: " + subject);
        toServer(out, "");
        toServer(out, text);
        toServer(out, ".");
        fromServer(in, MAIL_ACTION_OK_CODE);
        toServer(out, "QUIT");
        fromServer(in, SERVICE_CLOSING_CODE);
        
        sock.close();
    }
    
    private void fromServer(BufferedReader in, int expectedCode) throws Exception {
        String s = in.readLine();
        System.out.println("  => FROM SERVER: " + s);
        int code = 0;
        try {
            code = new Integer(s.substring(0,3));
        } catch (Exception e) {
            throw new MailClientException("Invalid response from SMTP server: '" + s + "'.");
        }
        if (code != expectedCode) {
            throw new MailClientException("Invalid status code returned from SMTP server. Got: " + code +
                                          " Expected: " + expectedCode);
        }
    }
    
    private void toServer(PrintWriter out, String s) throws Exception {
        System.out.println("  => TO SERVER: " + s);
        out.println(s);
        out.flush();
    }
    
    /*
     * java de.uni_freiburg.informatik.proglang.inetprog/ex1/SMTPServer smtp.informatik.uni-freiburg.de 25 wehr@informatik.uni-freiburg.de wehr@informatik.uni-freiburg.de 'Test Mail from Java' 'Hello World'
     */
    public static void main(String[] args) {
        if (args.length != 6) {
            System.err.println("Illegal commandline arguments, expected: SMTP-HOST SMTP-PORT FROM TO SUBJECT TEXT");
            System.exit(1);
        }
        try {
            InetAddress smtpAddr = InetAddress.getByName(args[0]); 
            int port = new Integer(args[1]);
            String from = args[2];
            String to = args[3];
            String subject = args[4];
            String text = args[5];
            SMTPServer client = new SMTPServer(smtpAddr, port);
            client.sendMail(from, to, subject, text);
        } catch(Exception e) {
            System.err.println(e.getMessage());
            System.exit(1);
        }
    }
}
