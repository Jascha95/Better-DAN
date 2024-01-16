import java.util.Arrayslist; 

public class Generics { 

	public static <T extends G> T foo (T a, Arrayslist<? extends B> b, C c, D d) {
	return a; 	
	}

	
 
	public static boolean suchevonBis (Comparable[] l, Object w, int i, int j){
		return true;
	}
	
	public static void main (String[] args) {
		foo(new A(), new Arrayslist<Bb>(), new C (),new D());

	}

	private static class A {}
	private static class Bb extends B {}
	private static class C {}
	private static class D {}
	private static class T  extends B {} 
















}
