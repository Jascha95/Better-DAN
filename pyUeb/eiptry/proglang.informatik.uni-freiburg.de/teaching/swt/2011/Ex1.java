public class Ex1 {
    public static int find_min(int[] a) {
	int x, i;
	x = a[0];
	for (i = 1; i < a.length;i ++) {
	    if (a[i] < x) x = a[i];
	}
	return x;
    }

    public static int[] insert(int[] x, int n) {
	int[] y = new int[x.length + 1];
	int i;
	for (i = 0; i < x.length; i++) {
	    if (n < x[i]) break;
	    y[i] = x[i];
	}
	y[i] = n;
	for (; i < x.length; i++) {
	    y[i+1] = x[i];
	}
	return y;
    }
}
