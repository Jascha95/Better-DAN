import java.net.*;
import java.io.*;

public class RawURLContent {

    private URLConnection uc;

    public RawURLContent (URL u)
	throws IOException {
	uc = u.openConnection ();
    }

    public byte[] getContent ()
	throws IOException {
	    int len = uc.getContentLength ();
	    if (len <= 0) {
		System.err.println ("Length cannot be determined");
		return new byte[0];
	    } else {
		byte[] rawContent = new byte [len];
		uc.getInputStream ().read (rawContent);
		return rawContent;
	    }
	}
}
