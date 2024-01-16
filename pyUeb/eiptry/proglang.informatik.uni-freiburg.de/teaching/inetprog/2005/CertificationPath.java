import java.net.*;
import java.io.*;
import javax.net.ssl.*;
import java.security.cert.*;

class CertificationPath {
    public static void main (String[] args) {
	try {
	    // Create the client socket
	    int port = 443;
	    String hostname = args[0];
	    SSLSocketFactory factory = HttpsURLConnection.getDefaultSSLSocketFactory();
	    SSLSocket socket = (SSLSocket)factory.createSocket(hostname, port);
	    
	    // Connect to the server
	    socket.startHandshake();
	    
	    // Retrieve the server's certificate chain
	    java.security.cert.Certificate[] serverCerts =
		socket.getSession().getPeerCertificates();

	    for (int i=0; i<serverCerts.length; i++) {
		System.out.println("Server certificate type: "+serverCerts[i].getType());
		if (serverCerts[i] instanceof X509Certificate) {
		    X509Certificate c = (X509Certificate)serverCerts[i];
		    System.out.println(" Subject: "+c.getSubjectDN());
		    System.out.println(" Issuer: "+c.getIssuerDN());
		}
	    }
	    
	    // Close the socket
	    socket.close();
	} catch (SSLPeerUnverifiedException e) {
	} catch (IOException e) {
	}
    }
}