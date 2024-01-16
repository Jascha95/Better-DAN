import java.util.*;

public class Ex2_Set<X> {
    private ArrayList<X> arr;
    
    public Ex2_Set() {
	arr = new ArrayList<X>();
    }
    
    public void add(X x) {
	for (int i = 0; i < arr.size(); i++) {
	    if (x.equals(arr.get(i))) return;
	}
	arr.add(x);
    }
    
    public boolean member(X x) {
	for (int i = 0; i < arr.size(); i++) {
	    if (x.equals(arr.get(i))) return true;
	}
	return false;
    }
    
    public int size() {
	return arr.size();
    }
    
    public void union(Ex2_Set<X> s) {
	for (int i = 0; i < s.arr.size(); i++) {
	    add(s.arr.get(i));
	}
    }
}
