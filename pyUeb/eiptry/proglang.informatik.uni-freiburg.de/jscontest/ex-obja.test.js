var h = (function  () {
      function h_own (x) {
            var g9473bfaefc4212ecaab376b0322404371 = TESTS.assertParams(["g9473bfaefc4212ecaab376b0322404370"], arguments, h_own);
            if (((x && x.p) && x.quest)) return g9473bfaefc4212ecaab376b0322404371.assertReturn("true");;
            return g9473bfaefc4212ecaab376b0322404371.assertReturn(false);
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
    var c_0 = TESTS.Function([TESTS.PObject(["quest", "p"])], TESTS.Boolean);
      TESTS.add("h", h, c_0, 1000. );
      TESTS.setVar("g9473bfaefc4212ecaab376b0322404370", c_0);
  })();