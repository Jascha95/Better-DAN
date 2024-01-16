var LinkedList = JSConTest.tests.addContracts("f_LinkedList0", JSConTest.tests.setVar("f_LinkedList0", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  () {
      JSConTest.effects.propAss(this, "_length", 0. );
        JSConTest.effects.propAss(this, "_head", null);
    }), []), (function LinkedList () {
      this._length = 0. ;
        this._head = null;
    }), false)), [], []);
  var add = JSConTest.tests.addContracts("f_add1", JSConTest.tests.setVar("f_add1", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (data) {
      var node = JSConTest.effects.fixObjectLiteral({data : data, next : null}),
            current;
        if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "_head")) === null))) {
            JSConTest.effects.propAss(this, "_head", node);
          }
          else {
            current = JSConTest.effects.propAcc(this, "_head");
            while (JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "next"))) {
              current = JSConTest.effects.propAcc(current, "next");
            }
            JSConTest.effects.propAss(current, "next", node);
          };
        JSConTest.effects.unOp(1. , this, "_length");
    }), ["data"]), ["c_2"], "f_add1"), (function add (data) {
      var node = {data : data, next : null},
            current;
        if ((this._head === null)) {
            this._head = node;
          }
          else {
            current = this._head;
            while (current.next) {
              current = current.next;
            }
            current.next = node;
          };
        this._length++ ;
    }), true)), [{contract : JSConTest.tests.setVar("c_2", JSConTest.contracts.Method(ll, [JSConTest.contracts.Integer], JSConTest.contracts.Undefined, [], "f_add1")), count : 50. }], []);
  var item = JSConTest.tests.addContracts("f_item3", JSConTest.tests.setVar("f_item3", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (index) {
      if (JSConTest.effects.unbox(((JSConTest.effects.unbox(index) > 1. ) && (JSConTest.effects.unbox(index) < JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "_length")))))) {
            var current = JSConTest.effects.propAcc(this, "_head"),
              i = 0. ;
            while (JSConTest.effects.unbox((JSConTest.effects.unbox(index) > JSConTest.effects.unbox(i++ )))) {
              current = JSConTest.effects.propAcc(current, "next");
            }
            return JSConTest.effects.propAcc(current, "data");
          }
          else {
            return null;
          };
    }), ["index"]), ["c_4"], "f_item3"), (function item (index) {
      if (((index >  -1. ) && (index < this._length))) {
            var current = this._head,
              i = 0. ;
            while ((index > i++ )) {
              current = current.next;
            }
            return current.data;
          }
          else {
            return null;
          };
    }), true)), [{contract : JSConTest.tests.setVar("c_4", JSConTest.contracts.Method(ll, [JSConTest.contracts.Length], JSConTest.contracts.Union(JSConTest.contracts.Integer, JSConTest.contracts.Null), [], "f_item3")), count : 50. }], []);
  var remove = JSConTest.tests.addContracts("f_remove5", JSConTest.tests.setVar("f_remove5", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (index) {
      if (JSConTest.effects.unbox(((JSConTest.effects.unbox(index) > 1. ) && (JSConTest.effects.unbox(index) < JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "_length")))))) {
            var current = JSConTest.effects.propAcc(this, "_head"),
              previous,
              i = 0. ;
            if (JSConTest.effects.unbox((JSConTest.effects.unbox(index) === 0. ))) {
              JSConTest.effects.propAss(this, "_head", JSConTest.effects.propAcc(current, "next"));
            }
            else {
              while (JSConTest.effects.unbox((JSConTest.effects.unbox(index) > JSConTest.effects.unbox(i++ )))) {
                previous = current;
                current = JSConTest.effects.propAcc(current, "next");
              }
              JSConTest.effects.propAss(previous, "next", JSConTest.effects.propAcc(current, "next"));
            };
            JSConTest.effects.unOp(2. , this, "_length");
            return JSConTest.effects.propAcc(current, "data");
          }
          else {
            return null;
          };
    }), ["index"]), ["c_6"], "f_remove5"), (function remove (index) {
      if (((index >  -1. ) && (index < this._length))) {
            var current = this._head,
              previous,
              i = 0. ;
            if ((index === 0. )) {
              this._head = current.next;
            }
            else {
              while ((index > i++ )) {
                previous = current;
                current = current.next;
              }
              previous.next = current.next;
            };
            this._length-- ;
            return current.data;
          }
          else {
            return null;
          };
    }), true)), [{contract : JSConTest.tests.setVar("c_6", JSConTest.contracts.Method(ll, [JSConTest.contracts.Length], JSConTest.contracts.Union(JSConTest.contracts.Integer, JSConTest.contracts.Null), [], "f_remove5")), count : 50. }], []);
  var size = JSConTest.tests.addContracts("f_size7", JSConTest.tests.setVar("f_size7", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
      return JSConTest.effects.propAcc(this, "_length");
    }), []), ["c_8"], "f_size7"), (function size () {
      return this._length;
    }), true)), [{contract : JSConTest.tests.setVar("c_8", JSConTest.contracts.Method(ll, [], JSConTest.contracts.Integer, [], "f_size7")), count : 50. }], []);
  var toArray = JSConTest.tests.addContracts("f_toArray9", JSConTest.tests.setVar("f_toArray9", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
      var result = [],
            current = JSConTest.effects.propAcc(this, "_head");
        while (JSConTest.effects.unbox(current)) {
            JSConTest.effects.mCall(result, "push", [current.data]);
            current = JSConTest.effects.propAcc(current, "next");
          }
        return result;
    }), []), ["c_10"], "f_toArray9"), (function toArray () {
      var result = [],
            current = this._head;
        while (current) {
            result.push(current.data);
            current = current.next;
          }
        return result;
    }), true)), [{contract : JSConTest.tests.setVar("c_10", JSConTest.contracts.Method(ll, [], JSConTest.contracts.Array(JSConTest.contracts.Integer), [], "f_toArray9")), count : 50. }], []);
  var toString = JSConTest.tests.addContracts("f_toString11", JSConTest.tests.setVar("f_toString11", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
      return JSConTest.effects.mCall(JSConTest.effects.mCall(this, "toArray", []), "toString", []);
    }), []), ["c_12"], "f_toString11"), (function toString () {
      return this.toArray().toString();
    }), true)), [{contract : JSConTest.tests.setVar("c_12", JSConTest.contracts.Method(ll, [], JSConTest.contracts.String, [], "f_toString11")), count : 50. }], []);
  LinkedList.prototype = {constructor : LinkedList, add : add, size : size, toArray : toArray, toString : toString};