import java.io.*;

public interface DialogHandler {
    // @return false to exit the server loop
    boolean talk (BufferedReader br, PrintWriter pw);
}
