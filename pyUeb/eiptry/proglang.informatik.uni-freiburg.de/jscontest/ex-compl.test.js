var fut_1 = (function  () {
      function fut_1_own (x, y, z) {
            var ga94a16336a4140fb53e0d5f60eb1a4511 = TESTS.assertParams(["ga94a16336a4140fb53e0d5f60eb1a4510"], arguments, fut_1_own);
            if (((((x * 3. ) + 5. ) == ((y * 5. ) + 4. )) && (((x * 2. ) - 1. ) == ((z * 9. ) - 1. )))) return ga94a16336a4140fb53e0d5f60eb1a4511.assertReturn("true");;
            return ga94a16336a4140fb53e0d5f60eb1a4511.assertReturn(false);
          };
        (function  () {
          function fut_1 (x, y, z) {
                if (((((x * 3. ) + 5. ) == ((y * 5. ) + 4. )) && (((x * 2. ) - 1. ) == ((z * 9. ) - 1. )))) return "true";;
                return false;
              };
            TESTS.overrideToStringOfFunction(fut_1_own, fut_1);
        })();
        return fut_1_own;
    })();
  (function  () {
    var c_0 = TESTS.Function([TESTS.AInteger([9. , 1. , 2. , 4. , 5. , 3. ]), TESTS.AInteger([9. , 1. , 2. , 4. , 5. , 3. ]), TESTS.AInteger([9. , 1. , 2. , 4. , 5. , 3. ])], TESTS.Boolean);
      TESTS.add("fut_1", fut_1, c_0, 1000000. );
      TESTS.setVar("ga94a16336a4140fb53e0d5f60eb1a4510", c_0);
  })();
