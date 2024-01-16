/**
 * Ein unbalancierter Binärsuchbaum
 */
public class BinTree {
	// Test program
	public static void main(String[] args) {
		BinTree t = new BinTree();
		final int NUMS = 4000;
		final int GAP = 37;

		System.out.println("Fuege Elemente hinzu");

		for (int i = GAP; i != 0; i = (i + GAP) % NUMS)
			t.put(i);
		
		System.out.println(t.toString() + "\nEntferne Elemente");

		for (int i = 1; i < NUMS; i += 2)
			t.remove(i);
		
		System.out.println(t.toString());
	}

	/** Die Wurzel */
	protected BinaryNode root;

	/**
	 * Kontruktor für einen Baum
	 */
	public BinTree() {
	}

	/**
	 * Leert den Baum
	 */
	public void clear() {
		root = null;
	}

	private BinaryNode findMin(BinaryNode t) {
		if (t != null)
			while (t.left != null)
				t = t.left;

		return t;
	}


//	/**
//	 * Gibt den kleinsten Schlüssel zurück
//	 */
//	@Override
//	public int firstKey() {
//		return (root == null ? null : firstKey(root));
//	}
//
//	private int firstKey(BinaryNode node) {
//		return (node.left == null ? node.key : firstKey(node.left));
//	}
}
