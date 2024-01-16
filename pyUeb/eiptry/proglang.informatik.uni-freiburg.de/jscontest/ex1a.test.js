var f = (function  () {
      function f_own (x) {
            TESTS.setVar("gd0644080ed53c013d9a8f3fd9a75b1bf1", TESTS.assertParams(["gd0644080ed53c013d9a8f3fd9a75b1bf0"], arguments, f_own));
            return TESTS.getVar("gd0644080ed53c013d9a8f3fd9a75b1bf1").assertReturn((2.  * x));
          };
        (function  () {
          function f (x) {
                return (2.  * x);
              };
            TESTS.overrideToStringOfFunction(f_own, f);
        })();
        return f_own;
    })();
  var p = (function  () {
      function p_own (x, y) {
            TESTS.setVar("gd0644080ed53c013d9a8f3fd9a75b1bf3", TESTS.assertParams(["gd0644080ed53c013d9a8f3fd9a75b1bf2"], arguments, p_own));
            if ((x != y)) {
                if ((f(x) == (x + 10. ))) return TESTS.getVar("gd0644080ed53c013d9a8f3fd9a75b1bf3").assertReturn("true");;
              };
            return TESTS.getVar("gd0644080ed53c013d9a8f3fd9a75b1bf3").assertReturn(false);
          };
        (function  () {
          function p (x, y) {
                if ((x != y)) {
                    if ((f(x) == (x + 10. ))) return "true";;
                  };
                return false;
              };
            TESTS.overrideToStringOfFunction(p_own, p);
        })();
        return p_own;
    })();
  (function  () {
    var c_0 = TESTS.Function([TESTS.Integer], TESTS.Integer);
      TESTS.add("f", f, c_0, 1000. );
      TESTS.setVar("gd0644080ed53c013d9a8f3fd9a75b1bf0", c_0);
  })();
  (function  () {
    var c_1 = TESTS.Function([TESTS.AInteger([10. ]), TESTS.Integer], TESTS.Boolean);
      TESTS.add("p", p, c_1, 1000. );
      TESTS.setVar("gd0644080ed53c013d9a8f3fd9a75b1bf2", c_1);
  })();