package de.uni_freiburg.informatik.proglang.inetprog.ex2;

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.PrintWriter;

import javax.net.ssl.SSLServerSocket;
import javax.net.ssl.SSLServerSocketFactory;
import javax.net.ssl.SSLSocket;

public class SecureEchoServer {
    public static void main(String[] args) {
        if (args.length != 2) {
            System.err.println("expected arguments: SERVERKEYSTORE PORT");
            System.exit(1);
        }
        
        System.setProperty("javax.net.ssl.keyStore", args[0]);
        System.setProperty("javax.net.ssl.keyStorePassword", "abcdef");
        
        try {
            int port = Integer.parseInt(args[1]);
            SSLServerSocketFactory sf = (SSLServerSocketFactory) SSLServerSocketFactory.getDefault();
            SSLServerSocket ss = (SSLServerSocket) sf.createServerSocket(port);

            while (true) {
                SSLSocket sock = (SSLSocket) ss.accept();
                System.out.println("connection from " + sock.getInetAddress());
                BufferedReader in = new BufferedReader(new InputStreamReader(sock.getInputStream()));
                String line = in.readLine();
                System.out.println("read " + line);
                PrintWriter out = new PrintWriter(sock.getOutputStream());
                out.print("ECHO: " + line.toUpperCase());
                out.flush();
                sock.close();
                System.out.println("client " + sock.getInetAddress() + " served");
            }
        } catch (IOException e) {
            System.err.println(e);
            e.printStackTrace();
        }
    }
}
