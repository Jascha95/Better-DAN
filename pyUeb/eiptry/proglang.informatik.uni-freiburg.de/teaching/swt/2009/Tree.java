class Tree {

    static class TreeNode {
	TreeNode left;
	TreeNode right;
	int data;
	
	public TreeNode (int value) {
	    data = value;
	}

	public String toString() {
	    return "Node(" + left + ", " + data + ", " + right + ")";
	}
    }

    TreeNode root;

    public void insert(int value) {
	if (root == null) {
	    root = new TreeNode (value);
	    return;
	}
	TreeNode last = null;
	TreeNode next = root;
	while (next != null) {
	    last = next;
	    if (value < next.data) {
		next = next.left;
	    } else if (value > next.data) {
		next = next.right;
	    }
	}
	if (value < last.data) {
	    last.left = new TreeNode (value);
	} else {
	    last.right = new TreeNode (value);
	}
    }

    public String toString() {
	return "Tree " + root;
    }

    
    public static void main (String[] args) {
	Tree t = new Tree();
	for (String s : args) {
	    t.insert(Integer.parseInt (s));
	}
	System.out.println (t);
    }
}
