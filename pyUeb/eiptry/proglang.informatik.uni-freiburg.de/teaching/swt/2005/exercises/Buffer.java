public class Buffer implements Cloneable {

  protected int in,out;
  protected Object[] buf;

  public Buffer (int anzahl) {
    buf = new Object[anzahl];
  }

  public boolean empty() {
    return in - out == 0;
  }

  public boolean full() {
    return in - out == buf.length;
  }

  public void add(Object o) {
    buf[in % buf.length] = o;
    in++;
  }

  public Object remove() {
    Object o = buf[out % buf.length];
    out++;
    return o;
  } 

  public boolean contains(Object o) {
    boolean found = false;
    for (int i = 0; i < buf.length; i++) 
      if (buf[i].equals(o)) 
        found = true;
    return found;
  }   
}
