import java.util.*;
import javax.mail.*;
import javax.mail.internet.*;

class ReadJavaMail {
    public static void main (String[] arg) {
	String incoming = arg[0];
	String username = arg[1];
	String password = arg[2];

	try {
	    // obtain a session
	    Properties props = new Properties();
	    Session session = Session.getDefaultInstance (props);

	    // get a store
	    Store store = session.getStore ("pop3");
	    store.connect (incoming, username, password);

	    // get the INBOX folder
	    Folder folder = store.getFolder ("INBOX");
	    folder.open (Folder.READ_ONLY);
	    Message[] msgs = folder.getMessages();

	    // application code: print statistics
	    System.out.println ("Statistics for INBOX@" + incoming);
	    System.out.println (msgs.length + " messages");
	    for (int i=0; i<msgs.length; i++) {
		System.out.println ("Message " + i + ":");
		System.out.println (((MimeMessage)msgs[i]).getContent());
		System.out.println ("-----------");
	    }

	    // cleanup
	    folder.close (false); // keep all messages
	    store.close ();
	}
	catch (Exception e) {
	    System.out.println ("!!!! Exception: " + e.toString());
	}
    }
}
