import java.io.*;
import java.util.*;
import javax.mail.*;

class MyAuthenticator extends Authenticator {
    public PasswordAuthentication getPasswordAuthentication() {
	String username = null, password = null;
    BufferedReader reader =
	new BufferedReader (new InputStreamReader(System.in));
    try {
	System.out.print ("Username: ");
	username = reader.readLine();
	System.out.print ("Password: ");
	password = reader.readLine();
    } catch (IOException e) {
    }
    return new PasswordAuthentication(username, password);
  }
}