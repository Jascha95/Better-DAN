var h = (function  () {
      function h_own (x) {
            var g6926414a7cdc744672c1a1603d6c1a3c1 = TESTS.assertParams(["g6926414a7cdc744672c1a1603d6c1a3c0"], arguments, h_own);
            if (((x && x.p) && x.quest)) return g6926414a7cdc744672c1a1603d6c1a3c1.assertReturn("true");;
            return g6926414a7cdc744672c1a1603d6c1a3c1.assertReturn(false);
          };
        (function  () {
          function h (x) {
                if (((x && x.p) && x.quest)) return "true";;
                return false;
              };
            TESTS.overrideToStringOfFunction(h_own, h);
        })();
        return h_own;
    })();
  (function  () {
    var c_0 = TESTS.Function([TESTS.Object], TESTS.Boolean);
      TESTS.add("h", h, c_0, 10000. );
      TESTS.setVar("g6926414a7cdc744672c1a1603d6c1a3c0", c_0);
  })();
