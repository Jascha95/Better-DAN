package de.uni_freiburg.informatik.proglang.inetprog.ex2;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.net.ServerSocket;
import java.net.Socket;
import java.net.URL;
import java.net.URLConnection;
import java.util.HashMap;
import java.util.Map;

public class HTTPProxy {

    /* ------------------------------------------------------------------- */
    /** Copy Stream in to Stream for byteCount bytes or until EOF or exception.
     * Stolen from http://www.koders.com/java/fid4326DB0888AA1455A30C2B763A9D71FD7FAD62A3.aspx
     */
    public static void copy(InputStream in,
                            OutputStream out) throws IOException {     
        int bufferSize = 8192;
        byte buffer[] = new byte[bufferSize];
        
        while (true) {
            int len=in.read(buffer,0,bufferSize);
            if (len < 0)
                break;
            out.write(buffer,0,len);
        }
    }
    
    // returns the URL
    private static URL parseStatusLine(BufferedReader in) throws IOException {
        String line = in.readLine();
        String[] tokens = line.split("\\s");
        return new URL(tokens[1]);
    }
    
    private static Map<String,String> parseHeaders(BufferedReader in) throws IOException {
        Map<String,String> m = new HashMap<String,String>();
        for (String line = in.readLine(); line != null && !"".equals(line);
             line = in.readLine()) {
             int i = line.indexOf(':');
             String name = line.substring(0, i);
             String value = line.substring(i+1);
             m.put(name, value);
        }
        return m;
    }
     
    private static void writeToBrowser(OutputStream out, URL url, Map<String,String> headers) throws IOException {
        System.err.println("writing " + url + " to browser");
        URLConnection con = url.openConnection();
        for (Map.Entry<String,String> entry : headers.entrySet()) {
            con.setRequestProperty(entry.getKey(), entry.getValue());    
        }
        con.connect();
        try {
            InputStream in = con.getInputStream();
            Writer w = new OutputStreamWriter(out, "ascii");
            for (int i = 0; ; i++) {
                String name = con.getHeaderFieldKey(i);
                String val = con.getHeaderField(i);
                if (val == null) {
                    break;
                }
                if (name != null) {
                    w.write(name + ":");
                }
                System.err.println(name + ": " + val);
                w.write(val + "\r\n");
            }
            w.write("\r\n");
            w.flush();
            copy(in, out);
            out.close();
        } catch(FileNotFoundException e) {
            System.err.println("Opening connection to " + url + "failed: " + e);
            String html = "<!DOCTYPE HTML PUBLIC \"-//IETF//DTD HTML 2.0//EN\">" +
                          "<HTML><HEAD>" +
                          "<TITLE>404 Not Found</TITLE>" +
                          "</HEAD><BODY>" +
                          "<H1>Not Found</H1>" +
                          "<P>The requested URL " + url + " was not found on this server.</P>" +
                          "</BODY></HTML>";
            Writer w = new OutputStreamWriter(out, "ascii");
            w.write("HTTP/1.x 404 Not Found\r\n");
            w.write("Content-Type: text/html; charset=ascii\r\n");
            w.write("Content-Length: " + html.length() + "\r\n\r\n");
            w.write(html);
            w.flush();
            out.close();
        }
    }
    
    public static void listen(int port) throws Exception {
        ServerSocket ssock = new ServerSocket(port);
        while (true) {
            System.err.println("Waiting for connections...");
            Socket sock = ssock.accept();
            try {
                System.err.println("Connection established from " + sock.getInetAddress());
                BufferedReader in = new BufferedReader(new InputStreamReader(sock.getInputStream()));         
                URL url = parseStatusLine(in);
                Map<String,String> headers = parseHeaders(in);
                writeToBrowser(sock.getOutputStream(), url, headers);
                sock.close();
            } catch(Exception e) {
                System.err.println(e.getMessage());
                e.printStackTrace();
            }
        }
    }
    
    public static void main(String[] args) {
        if (args.length != 1) {
            System.err.println("Illegal commandline arguments, expected: PORT");
            System.exit(1);
        }
        int port = new Integer(args[0]);
        try {
            listen(port);
        } catch(Exception e) {
            System.err.println(e.getMessage());
            e.printStackTrace();
        }
    }
}
