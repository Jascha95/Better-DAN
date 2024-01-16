import java.util.*;
import javax.mail.*;
import javax.mail.internet.*;

class SendJavaMail {
    public static void main (String[] arg) {
	String outgoing = arg[0];
	String from = arg[1];
	String to = arg[2];

	try {
	    // obtain a session for the outgoing SMTP server
	    Properties props = System.getProperties();
	    props.put ("mail.smtp.host", outgoing);
	    Session session = Session.getDefaultInstance (props);
	    
	    // create a new message object
	    MimeMessage msg = new MimeMessage (session);
	    // headers and contents are properties
	    msg.setText("Nobody overtook my car...");
	    msg.setSubject ("Highway Star");
	    
	    // must attach addresses
	    Address fromaddr = new InternetAddress (from, "Peter Thiemann");
	    msg.setFrom (fromaddr);
	    
	    Address toaddr = new InternetAddress (from, "Peter Thiemann");
	    msg.addRecipient (Message.RecipientType.TO, toaddr);
	    
	    // send it off via SMTP
	    Transport transport = session.getTransport("smtp");
	    transport.connect();
	    transport.sendMessage (msg, msg.getAllRecipients());
	    
	    transport.close();
	}
	catch (Exception e) {
	}
    }
}