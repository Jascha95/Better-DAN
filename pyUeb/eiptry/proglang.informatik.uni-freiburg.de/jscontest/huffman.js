var ht = 
  (function () {
    function generate() {
      function genRandomLeaf() {
        return makeHuffmanLeaf(TESTS.genStringL(1), TESTS.genNInt(0,1));
      };
      function genRandomNode(l,r) {
        return makeHuffmanNode([],TESTS.genNInt(0,1),l,r);
      };
      function cdes() { return "makeHuffmanNode"; };
      var gN = { getcdes: cdes, arity: 2, f: genRandomNode};
      return TESTS.genTree(isHuffmanTree,[genRandomLeaf],[gN],0.5,true);
    };
    var gen = TESTS.restrictTo(isHuffmanNode,generate);
    return new TESTS.newSContract(isHuffmanTree,gen,"HuffmanTree");
  })();


/** Top -> bool #Tests:50 */
function isHuffmanLeaf(v) {
  if ((v) && v.isHuffmanLeaf) return true;
  return false;
};

/** Top -> bool #Tests:50 */
function isHuffmanNode(v) {
  if ((v) && v.isHuffmanNode) return true;
  return false;
};

/** Top -> bool #Tests:50 | js:ht -> true ~noAsserts #Tests:50 */ 
function isHuffmanTree(v) {
  if (isHuffmanLeaf(v)) return true;
  return isHuffmanNode(v);
};    

/** (string,[0;1.0]) -> js:ht #Tests:50 */
function makeHuffmanLeaf (s,w) {
  return { isHuffmanLeaf: true, s : s, weight: w };
};

/** ([string],[0;1.0],js:ht,js:ht) -> js:ht #Tests:50 */
function makeHuffmanNode(sl,w,l,r) {
  return { isHuffmanNode: true, sl: sl, weight: w, left: l, right: r };
};

/** (js:ht,[[0;1]]) -> string */
function huffmandecode (htroot,bits) {
  function decode(htree,bits) {
    if (htree && (htree.isHuffmanLeaf)) {
      var s = htree.s;
      var r = decode(htroot,bits);
      var result = s.concat(r);
      return result;
    } else {
      if (htree && (htree.isHuffmanNode)) {
        if (bits.length < 1) return "";
        var b = bits[0];
        var bitrest = [];
        for (var i = 1; i<bits.length; i++) {
          bitrest.push(bits[i]);
        };
        if (b === 0) {
          return decode(htree.left,bitrest);
        } else {
          return decode(htree.right,bitrest);
        };
      };
    };
  };
  return decode(htroot,bits);
};

/** (js:ht,[[0;1]]) -> [string] */
function decode(ht,bits) {
  if (ht && (ht.isHuffmanLeaf)) {
    return [ht.s];
  } else {
    if (ht && (ht.isHuffmanNode)) {
      if (bits.length < 1) return [];
      var b = bits[0];
      var bitrest = [];
      for (var i = 1; i<bits.length; i++) {
        bitrest.push(bits[i]);
      };
      if (b === 0) {
        return decode(ht.left,bitrest);
      } else {
        return decode(ht.right,bitrest);
      };
    };
  };
};
