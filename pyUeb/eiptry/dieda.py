def search(tree, item):
    if tree is None:
        return False
    elif tree[0] == item:
        return True
    elif tree[0] > item:
        return search(tree[1], item)
    else:
        return search(tree[2], item)

nums = [10, [5,[1, None,None], None],[15, [12,None,None],None, None]]

print(search(nums,3))


def height(tree):
    if tree is None:
        return -1 
    else:
        return(max(height(tree.left),height(tree.right))+1)
