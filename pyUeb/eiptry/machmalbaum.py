class Node:
    def __init__(self, L, R, n):
        self.left = L
        self.right = R
        self.value = n

    def __repr__(self):
        return f"node {self.value}"







def tree_0(tree: Node) :
    if tree is None:
        return "mach mal full"
    else: 
        l_str = tree_0(tree.left)
        r_str = tree_0(tree.right)
        return "mach mal node rein"
