package de.uni_freiburg.informatik.proglang.inetprog.ex2;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintStream;
import java.io.PrintWriter;

import javax.net.ssl.SSLSocket;
import javax.net.ssl.SSLSocketFactory;

public class SecureEchoClient {
    public static void main(String[] args) {
        if (args.length != 4) {
            System.err.println("expected arguments: TRUSTSTORE HOST PORT TEXT");
            System.exit(1);
        }
        
        String trustStore = args[0];
        System.setProperty("javax.net.ssl.trustStore", trustStore);
        System.setProperty("javax.net.ssl.trustStorePassword", "123456");
        
        try {
            String host = args[1];
            int port = Integer.parseInt(args[2]);
            String text = args[3];
            
            SSLSocketFactory sf = (SSLSocketFactory) SSLSocketFactory.getDefault();
            SSLSocket sock = (SSLSocket) sf.createSocket(host, port);

            PrintWriter out = new PrintWriter(sock.getOutputStream());            
            out.write(text + "\n");
            out.flush();
            System.out.println("wrote " + text);
            
            BufferedReader in = new BufferedReader(new InputStreamReader(sock.getInputStream()));
            String answ = in.readLine();
            System.out.println(answ);
            
            sock.close();
        } catch (IOException e) {
            System.err.println(e);
        }
    }
}
