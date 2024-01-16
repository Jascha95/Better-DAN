var f = (function  () {
      function f_own (x) {
            TESTS.setVar("ge473df763233e88b667a332b18ef64b71", TESTS.assertParams(["ge473df763233e88b667a332b18ef64b70"], arguments, f_own));
            return TESTS.getVar("ge473df763233e88b667a332b18ef64b71").assertReturn((2.  * x));
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
            TESTS.setVar("ge473df763233e88b667a332b18ef64b73", TESTS.assertParams(["ge473df763233e88b667a332b18ef64b72"], arguments, p_own));
            if ((x != y)) {
                if ((f(x) == (x + 10. ))) return TESTS.getVar("ge473df763233e88b667a332b18ef64b73").assertReturn("true");;
              };
            return TESTS.getVar("ge473df763233e88b667a332b18ef64b73").assertReturn(false);
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
  var g = (function  () {
      function g_own (x, y) {
            TESTS.setVar("ge473df763233e88b667a332b18ef64b75", TESTS.assertParams(["ge473df763233e88b667a332b18ef64b74"], arguments, g_own));
            return TESTS.getVar("ge473df763233e88b667a332b18ef64b75").assertReturn((f((x * "3O")) == 60. ));
          };
        (function  () {
          function g (x, y) {
                return (f((x * "3O")) == 60. );
              };
            TESTS.overrideToStringOfFunction(g_own, g);
        })();
        return g_own;
    })();
  (function  () {
    var c_0 = TESTS.Function([TESTS.Integer], TESTS.Integer);
      TESTS.add("f", f, c_0, 1000. );
      TESTS.setVar("ge473df763233e88b667a332b18ef64b70", c_0);
  })();
  (function  () {
    var c_1 = TESTS.Function([TESTS.Integer, TESTS.Integer], TESTS.Boolean);
      TESTS.add("p", p, c_1, 1000. );
      TESTS.setVar("ge473df763233e88b667a332b18ef64b72", c_1);
  })();
  (function  () {
    var c_2 = TESTS.Function([TESTS.Integer, TESTS.Integer], TESTS.Boolean);
      TESTS.add("g", g, c_2, 1000. );
      TESTS.setVar("ge473df763233e88b667a332b18ef64b74", c_2);
  })();