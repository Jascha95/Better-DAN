var isHuffmanLeaf = (function  () {
      function isHuffmanLeaf_own (v) {
            var g522119a878e98b5759a47c001b815a8d1 = TESTS.assertParams(["g522119a878e98b5759a47c001b815a8d0"], arguments, isHuffmanLeaf_own);
            if ((v && v.isHuffmanLeaf)) return g522119a878e98b5759a47c001b815a8d1.assertReturn(true);;
            return g522119a878e98b5759a47c001b815a8d1.assertReturn(false);
          };
        (function  () {
          function isHuffmanLeaf (v) {
                if ((v && v.isHuffmanLeaf)) return true;;
                return false;
              };
            TESTS.overrideToStringOfFunction(isHuffmanLeaf_own, isHuffmanLeaf);
        })();
        return isHuffmanLeaf_own;
    })();
  var isHuffmanNode = (function  () {
      function isHuffmanNode_own (v) {
            var g522119a878e98b5759a47c001b815a8d3 = TESTS.assertParams(["g522119a878e98b5759a47c001b815a8d2"], arguments, isHuffmanNode_own);
            if ((v && v.isHuffmanNode)) return g522119a878e98b5759a47c001b815a8d3.assertReturn(true);;
            return g522119a878e98b5759a47c001b815a8d3.assertReturn(false);
          };
        (function  () {
          function isHuffmanNode (v) {
                if ((v && v.isHuffmanNode)) return true;;
                return false;
              };
            TESTS.overrideToStringOfFunction(isHuffmanNode_own, isHuffmanNode);
        })();
        return isHuffmanNode_own;
    })();
  var isHuffmanTree = (function  () {
      function isHuffmanTree_own (v) {
            var g522119a878e98b5759a47c001b815a8d6 = TESTS.assertParams(["g522119a878e98b5759a47c001b815a8d4"], arguments, isHuffmanTree_own);
            if (isHuffmanLeaf(v)) return g522119a878e98b5759a47c001b815a8d6.assertReturn(true);;
            return g522119a878e98b5759a47c001b815a8d6.assertReturn(isHuffmanNode(v));
          };
        (function  () {
          function isHuffmanTree (v) {
                if (isHuffmanLeaf(v)) return true;;
                return isHuffmanNode(v);
              };
            TESTS.overrideToStringOfFunction(isHuffmanTree_own, isHuffmanTree);
        })();
        return isHuffmanTree_own;
    })();
  var makeHuffmanLeaf = (function  () {
      function makeHuffmanLeaf_own (s, w) {
            var g522119a878e98b5759a47c001b815a8d8 = TESTS.assertParams(["g522119a878e98b5759a47c001b815a8d7"], arguments, makeHuffmanLeaf_own);
            return g522119a878e98b5759a47c001b815a8d8.assertReturn({isHuffmanLeaf : true, s : s, weight : w});
          };
        (function  () {
          function makeHuffmanLeaf (s, w) {
                return {isHuffmanLeaf : true, s : s, weight : w};
              };
            TESTS.overrideToStringOfFunction(makeHuffmanLeaf_own, makeHuffmanLeaf);
        })();
        return makeHuffmanLeaf_own;
    })();
  var makeHuffmanNode = (function  () {
      function makeHuffmanNode_own (sl, w, l, r) {
            var g522119a878e98b5759a47c001b815a8d10 = TESTS.assertParams(["g522119a878e98b5759a47c001b815a8d9"], arguments, makeHuffmanNode_own);
            return g522119a878e98b5759a47c001b815a8d10.assertReturn({isHuffmanNode : true, sl : sl, weight : w, left : l, right : r});
          };
        (function  () {
          function makeHuffmanNode (sl, w, l, r) {
                return {isHuffmanNode : true, sl : sl, weight : w, left : l, right : r};
              };
            TESTS.overrideToStringOfFunction(makeHuffmanNode_own, makeHuffmanNode);
        })();
        return makeHuffmanNode_own;
    })();
  var huffmandecode = (function  () {
      function huffmandecode_own (htroot, bits) {
            var g522119a878e98b5759a47c001b815a8d12 = TESTS.assertParams(["g522119a878e98b5759a47c001b815a8d11"], arguments, huffmandecode_own);
            function decode (htree, bits) {
                if ((htree && htree.isHuffmanLeaf)) {
                    var s = htree.s;
                    var r = decode(htroot, bits);
                    var result = s.concat(r);
                    return g522119a878e98b5759a47c001b815a8d12.assertReturn(result);
                  }
                  else {
                    if ((htree && htree.isHuffmanNode)) {
                      if ((bits.length < 1. )) return g522119a878e98b5759a47c001b815a8d12.assertReturn("");;
                      var b = bits[0. ];
                      var bitrest = [];
                      for (var i = 1. ; (i < bits.length); i++ )  {
                        bitrest.push(bits[i]);
                      };
                      if ((b === 0. )) {
                        return g522119a878e98b5759a47c001b815a8d12.assertReturn(decode(htree.left, bitrest));
                      }
                      else {
                        return g522119a878e98b5759a47c001b815a8d12.assertReturn(decode(htree.right, bitrest));
                      };
                    };
                  };
              };
            return g522119a878e98b5759a47c001b815a8d12.assertReturn(decode(htroot, bits));
          };
        (function  () {
          function huffmandecode (htroot, bits) {
                function decode (htree, bits) {
                    if ((htree && htree.isHuffmanLeaf)) {
                        var s = htree.s;
                        var r = decode(htroot, bits);
                        var result = s.concat(r);
                        return result;
                      }
                      else {
                        if ((htree && htree.isHuffmanNode)) {
                          if ((bits.length < 1. )) return "";;
                          var b = bits[0. ];
                          var bitrest = [];
                          for (var i = 1. ; (i < bits.length); i++ )  {
                            bitrest.push(bits[i]);
                          };
                          if ((b === 0. )) {
                            return decode(htree.left, bitrest);
                          }
                          else {
                            return decode(htree.right, bitrest);
                          };
                        };
                      };
                  };
                return decode(htroot, bits);
              };
            TESTS.overrideToStringOfFunction(huffmandecode_own, huffmandecode);
        })();
        return huffmandecode_own;
    })();
  var decode = (function  () {
      function decode_own (ht, bits) {
            var g522119a878e98b5759a47c001b815a8d14 = TESTS.assertParams(["g522119a878e98b5759a47c001b815a8d13"], arguments, decode_own);
            if ((ht && ht.isHuffmanLeaf)) {
                return g522119a878e98b5759a47c001b815a8d14.assertReturn([ht.s]);
              }
              else {
                if ((ht && ht.isHuffmanNode)) {
                  if ((bits.length < 1. )) return g522119a878e98b5759a47c001b815a8d14.assertReturn([]);;
                  var b = bits[0. ];
                  var bitrest = [];
                  for (var i = 1. ; (i < bits.length); i++ )  {
                    bitrest.push(bits[i]);
                  };
                  if ((b === 0. )) {
                    return g522119a878e98b5759a47c001b815a8d14.assertReturn(decode(ht.left, bitrest));
                  }
                  else {
                    return g522119a878e98b5759a47c001b815a8d14.assertReturn(decode(ht.right, bitrest));
                  };
                };
              };
          };
        (function  () {
          function decode (ht, bits) {
                if ((ht && ht.isHuffmanLeaf)) {
                    return [ht.s];
                  }
                  else {
                    if ((ht && ht.isHuffmanNode)) {
                      if ((bits.length < 1. )) return [];;
                      var b = bits[0. ];
                      var bitrest = [];
                      for (var i = 1. ; (i < bits.length); i++ )  {
                        bitrest.push(bits[i]);
                      };
                      if ((b === 0. )) {
                        return decode(ht.left, bitrest);
                      }
                      else {
                        return decode(ht.right, bitrest);
                      };
                    };
                  };
              };
            TESTS.overrideToStringOfFunction(decode_own, decode);
        })();
        return decode_own;
    })();
  var ht = (function  () {
    function generate () {
          function genRandomLeaf () {
              return makeHuffmanLeaf(TESTS.genStringL(1. ), TESTS.genNInt(0. , 1. ));
            };
          function genRandomNode (l, r) {
              return makeHuffmanNode([], TESTS.genNInt(0. , 1. ), l, r);
            };
          function cdes () {
              return "makeHuffmanNode";
            };
          var gN = {getcdes : cdes, arity : 2. , f : genRandomNode};
          return TESTS.genTree(isHuffmanTree, [genRandomLeaf], [gN], 0.5 , true);
        };
      var gen = TESTS.restrictTo(isHuffmanNode, generate);
      return new TESTS.newSContract(isHuffmanTree, gen, "HuffmanTree");
  })();
  (function  () {
    var c_0 = TESTS.Function([TESTS.Top], TESTS.Boolean);
      TESTS.add("isHuffmanLeaf", isHuffmanLeaf, c_0, 50. );
      TESTS.setVar("g522119a878e98b5759a47c001b815a8d0", c_0);
  })();
  (function  () {
    var c_1 = TESTS.Function([TESTS.Top], TESTS.Boolean);
      TESTS.add("isHuffmanNode", isHuffmanNode, c_1, 50. );
      TESTS.setVar("g522119a878e98b5759a47c001b815a8d2", c_1);
  })();
  (function  () {
    var c_3 = TESTS.Function([ht], TESTS.True);
      TESTS.add("isHuffmanTree", isHuffmanTree, c_3, 50. );
      TESTS.setVar("g522119a878e98b5759a47c001b815a8d5", c_3);
  })();
  (function  () {
    var c_2 = TESTS.Function([TESTS.Top], TESTS.Boolean);
      TESTS.add("isHuffmanTree", isHuffmanTree, c_2, 50. );
      TESTS.setVar("g522119a878e98b5759a47c001b815a8d4", c_2);
  })();
  (function  () {
    var c_4 = TESTS.Function([TESTS.String, TESTS.NIntervall(0. , 1. )], ht);
      TESTS.add("makeHuffmanLeaf", makeHuffmanLeaf, c_4, 50. );
      TESTS.setVar("g522119a878e98b5759a47c001b815a8d7", c_4);
  })();
  (function  () {
    var c_5 = TESTS.Function([TESTS.Array(TESTS.String), TESTS.NIntervall(0. , 1. ), ht, ht], ht);
      TESTS.add("makeHuffmanNode", makeHuffmanNode, c_5, 50. );
      TESTS.setVar("g522119a878e98b5759a47c001b815a8d9", c_5);
  })();
  (function  () {
    var c_6 = TESTS.Function([ht, TESTS.Array(TESTS.IIntervall(0. , 1. ))], TESTS.String);
      TESTS.add("huffmandecode", huffmandecode, c_6, 1000. );
      TESTS.setVar("g522119a878e98b5759a47c001b815a8d11", c_6);
  })();
  (function  () {
    var c_7 = TESTS.Function([ht, TESTS.Array(TESTS.IIntervall(0. , 1. ))], TESTS.Array(TESTS.String));
      TESTS.add("decode", decode, c_7, 1000. );
      TESTS.setVar("g522119a878e98b5759a47c001b815a8d13", c_7);
  })();