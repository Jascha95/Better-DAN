var oc =
  (function () {
        function gen() {
            return new OrderedCollection();
        }
        function test(x) {
            return (x instanceof OrderedCollection);  
        }
        return new JSConTest.contracts.SContract(test,gen,"OrderedCollection");
     })();


var p =
     (function () {
        function gen() {
            return new Plan();
        }
        function test(x) {
            return (x instanceof Plan);  
        }
        return new JSConTest.contracts.SContract(test,gen,"Plan");
     })();

var pl =
     (function () {
        function gen() {
            return new Planner();
        }
        function test(x) {
            return (x instanceof Planner);  
        }
        return new JSConTest.contracts.SContract(test,gen,"Planner");
     })();

var v =
     (function () {
        function gen() {
            var init = JSConTest.gen.genInt();
            var n = JSConTest.gen.genPropString();
            return new Variable(n,init);
        }
        function test(x) {
            return (x instanceof Variable);  
        }
        return new JSConTest.contracts.SContract(test,gen,"Variable");
     })();



var uc = (function () {
        function gen() {             
          var i = JSConTest.gen.genBoolean();
          if (i) {
               return stc.gen();
          } else {
               return edc.gen();
          } 
        }
        function test(x) {
            return (x instanceof UnaryConstraint);  
        }
        return new JSConTest.contracts.SContract(test,gen,"UnaryConstraint");
     })();


var edc = (function () {
        function gen() {             
              var v1 = v.gen();
              var str = s.gen();
              return new EditConstraint(v1,str);
        }
        function test(x) {
            return (x instanceof EditConstraint);  
        }
        return new JSConTest.contracts.SContract(test,gen,"EditConstraint");
     })();


var stc = (function () {
        function gen() {             
              var v1 = v.gen();
              var str = s.gen();
              return new StayConstraint(v1,str);
        }
        function test(x) {
            return (x instanceof StayConstraint);  
        }
        return new JSConTest.contracts.SContract(test,gen,"StayConstraint");
     })();


var bc = (function () {
        function gen() {             
          var i = JSConTest.gen.genBoolean();
          if (i) {
               return scc.gen();
          } else {
               return eqc.gen();
          } 
        }
        function test(x) {
            return (x instanceof BinaryConstraint);  
        }
        return new JSConTest.contracts.SContract(test,gen,"BinaryConstraint");
     })();

var scc = (function () {
        function gen() {             
              var src = v.gen();
              var dest = v.gen();
              var scale = v.gen();
              var offset = v.gen();
              var strength = s.gen(); 
              return new ScaleConstraint(src, scale, offset, dest, strength);
        }
        function test(x) {
            return (x instanceof ScaleConstraint);  
        }
        return new JSConTest.contracts.SContract(test,gen,"ScaleConstraint");
     })();


var eqc = (function () {
        function gen() {             
              var v1 = v.gen();
              var v2 = v.gen();
              var str = s.gen();
              return new EqualityConstraint(v1,v2,str);
        }
        function test(x) {
            return (x instanceof EqualityConstraint);  
        }
        return new JSConTest.contracts.SContract(test,gen,"EqualityConstraint");
     })();

var c =
     (function () {
        function gen() {             
          var i = JSConTest.gen.genBoolean();
          if (i) {
               return uc.gen();
          } else {
               return bc.gen();
          } 

        }
        function test(x) {
            return (x instanceof Constraint);  
        }
        return new JSConTest.contracts.SContract(test,gen,"Constraint");
     })();

var nullc = JSConTest.contracts.Union(c,JSConTest.contracts.SContract(JSConTest.check.isNull,JSConTest.gen.genNull,"Null"));
var undefc = JSConTest.contracts.Union(c,JSConTest.contracts.SContract(JSConTest.check.isUndefined,JSConTest.gen.genUndefined,"Undef"));

var s0 =
     (function () {
        function gen() {
            return Strength.REQUIRED;
        }
        function test(x) {
            return (x === Strength.REQUIRED);  
        }
        return new JSConTest.contracts.SContract(test,gen,"Strength.Required");
     })();
    
var s1 =
     (function () {
        function gen() {
            return Strength.STONG_PREFERRED;
        }
        function test(x) {
            return (x === Strength.STONG_PREFERRED);  
        }
        return new JSConTest.contracts.SContract(test,gen,"Strength.Strong_Preferred");
     })();
var s2 =
     (function () {
        function gen() {
            return Strength.PREFERRED;
        }
        function test(x) {
            return (x === Strength.PREFERRED);  
        }
        return new JSConTest.contracts.SContract(test,gen,"Strength.Preferred");
     })();
var s3 =
     (function () {
        function gen() {
            return Strength.STRONG_DEFAULT;
        }
        function test(x) {
            return (x === Strength.STRONG_DEFAULT);  
        }
        return new JSConTest.contracts.SContract(test,gen,"Strength.Strong_Default");
     })();
var s4 =
     (function () {
        function gen() {
            return Strength.NORMAL;
        }
        function test(x) {
            return (x === Strength.NORMAL);  
        }
        return new JSConTest.contracts.SContract(test,gen,"Strength.Normal");
     })();
var s5 =
     (function () {
        function gen() {
            return Strength.WEAK_DEFAULT;
        }
        function test(x) {
            return (x === Strength.WEAK_DEFAULT);  
        }
        return new JSConTest.contracts.SContract(test,gen,"Strength.Weak_Default");
     })();

var s6 =
     (function () {
        function gen() {
            return Strength.WEAKEST;
        }
        function test(x) {
            return (x === Strength.WEAKEST);  
        }
        return new JSConTest.contracts.SContract(test,gen,"Strength.Weakest");
     })();

var s = JSConTest.contracts.Union(JSConTest.contracts.Union(JSConTest.contracts.Union(JSConTest.contracts.Union(JSConTest.contracts.Union(JSConTest.contracts.Union(s0,s1),s2),s3),s4),s5),s6);
var StrengthClass = JSConTest.contracts.SContract(function (x) { return x === Strength }, 
						  function () { return Strength },
						  "StrengthClass");
