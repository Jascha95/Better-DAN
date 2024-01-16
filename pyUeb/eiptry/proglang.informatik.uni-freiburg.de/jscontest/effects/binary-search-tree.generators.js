var bst = 
  (function () {
    function gen() {
      var tree = JSConTest.effects.newCall(BinarySearchTree,[]);
      var size = JSConTest.gen.genLength();
      while (size >= 0) {
        var item = JSConTest.gen.genInt();
        JSConTest.effects.mCall(tree,"add",[item]);
        size--;
      }      
      return tree;
    }
    function test(x) {
      return (x instanceof BinarySearchTree);  
    }
    return new JSConTest.contracts.SContract(test,gen,"BSTree");
  })();
