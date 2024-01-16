def tree_str (tree : Node): 
    Node = 0
    if tree is None:
        return "fill␣for␣empty"
    else:
        l_str = tree_str (tree.left)
        r_str = tree_str (tree.right) 
        return "fill␣for␣node"