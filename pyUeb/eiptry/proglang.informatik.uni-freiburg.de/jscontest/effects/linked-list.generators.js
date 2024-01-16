var ll = 
    (function () {
        function gen() {
          var list = JSConTest.effects.newCall(LinkedList,[]);
            var size = JSConTest.gen.genLength();
            while (size >= 0) {
                var item = JSConTest.gen.genInt();
                list.add(item);
                size--;   
            }
            return list;
        }
        function test(x) {
            return (x instanceof LinkedList);  
        }
        return new JSConTest.contracts.SContract(test,gen,"LList");
     })();


