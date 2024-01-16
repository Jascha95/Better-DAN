var array_access1 = JSConTest.tests.addContracts("f_array_access10", JSConTest.tests.setVar("f_array_access10", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (a, i, j, k) {
      JSConTest.effects.propAss(a, j, k);
        return JSConTest.effects.propAcc(a, i);
    }), ["a", "i", "j", "k"]), ["c_1"], "f_array_access10"), (function array_access1 (a, i, j, k) {
      a[j] = k;
        return a[i];
    }), true)), [{contract : JSConTest.tests.setVar("c_1", JSConTest.contracts.Function([JSConTest.contracts.Array(JSConTest.contracts.Top), JSConTest.contracts.Natural, JSConTest.contracts.Natural, JSConTest.contracts.Top], JSConTest.contracts.Top, [], "f_array_access10")), count : 50. }], []);
  var array_access2 = JSConTest.tests.addContracts("f_array_access22", JSConTest.tests.setVar("f_array_access22", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (obj, i, j, k) {
      var a = JSConTest.effects.propAcc(obj, "arr");
        JSConTest.effects.propAss(a, j, k);
        return JSConTest.effects.propAcc(a, i);
    }), ["obj", "i", "j", "k"]), ["c_3"], "f_array_access22"), (function array_access2 (obj, i, j, k) {
      var a = obj.arr;
        a[j] = k;
        return a[i];
    }), true)), [{contract : JSConTest.tests.setVar("c_3", JSConTest.contracts.Function([JSConTest.contracts.EObject([{name : "arr", contract : JSConTest.contracts.Array(JSConTest.contracts.Top)}]), JSConTest.contracts.Natural, JSConTest.contracts.Natural, JSConTest.contracts.Top], JSConTest.contracts.Top, [], "f_array_access22")), count : 50. }], []);
  var array_access3 = JSConTest.tests.addContracts("f_array_access34", JSConTest.tests.setVar("f_array_access34", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (obj, i, j, k) {
      a = JSConTest.effects.propAcc(JSConTest.effects.propAcc(obj, "obj"), "arr");
        JSConTest.effects.propAss(a, j, k);
        return JSConTest.effects.propAcc(a, i);
    }), ["obj", "i", "j", "k"]), ["c_5"], "f_array_access34"), (function array_access3 (obj, i, j, k) {
      a = obj.obj.arr;
        a[j] = k;
        return a[i];
    }), true)), [{contract : JSConTest.tests.setVar("c_5", JSConTest.contracts.Function([JSConTest.contracts.EObject([{name : "obj", contract : JSConTest.contracts.EObject([{name : "arr", contract : JSConTest.contracts.Array(JSConTest.contracts.Top)}])}]), JSConTest.contracts.Natural, JSConTest.contracts.Natural, JSConTest.contracts.Top], JSConTest.contracts.Top, [], "f_array_access34")), count : 50. }], []);
  var hashmap_access = JSConTest.tests.addContracts("f_hashmap_access6", JSConTest.tests.setVar("f_hashmap_access6", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (h, x, y, z) {
      JSConTest.effects.propAss(h, x, y);
        return JSConTest.effects.propAcc(h, z);
    }), ["h", "x", "y", "z"]), ["c_7"], "f_hashmap_access6"), (function hashmap_access (h, x, y, z) {
      h[x] = y;
        return h[z];
    }), true)), [{contract : JSConTest.tests.setVar("c_7", JSConTest.contracts.Function([JSConTest.contracts.Array(JSConTest.contracts.Top), JSConTest.contracts.String, JSConTest.contracts.Top, JSConTest.contracts.String], JSConTest.contracts.Top, [], "f_hashmap_access6")), count : 50. }], []);