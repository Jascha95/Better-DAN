public class BinSearch {

    public static int search( int array[], int target ) {

	int low = 0;
	int high = array.length;
	int mid;

	while ( low <= high ) {
	    mid = (low + high) / 2 ;
	    if ( target < array[ mid ] ) {
		high = mid - 1;
	    } else if ( target > array[ mid ] ) {
		low = mid + 1;
	    } else {
		return mid;
	    }
	}
	return -1;
    }

    /**
     * <code>main</code> method reads an array and target element from
     * input stream and calls <code>search</code>. We assume that
     * numbers are one digit long and non-negative. Occurrence of
     * any other character is taken to mean the empty array.
     *
     * @param args a <code>String</code> value
     */
    public static final void main(final String[] args) {
	char[] a = args[0].toCharArray();
	int[] b = new int[a.length];
	System.out.print("In { ");
	for (int i = 0; i < a.length; i++) {
	    b[i] = Character.digit(a[i],10);
	    if (b[i] < 0) {
		b = new int[0];
		break;
	    }
	    System.out.print(b[i] + " ");
	}
	int t = Character.digit((args[1].toCharArray())[0],10);
	System.out.println("} we look for " + t);

	System.out.println("Found at position " + search(b, t));
	
    }


}
