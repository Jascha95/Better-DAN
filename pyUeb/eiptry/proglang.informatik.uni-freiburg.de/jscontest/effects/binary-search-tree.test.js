var BinarySearchTree = JSConTest.tests.addContracts("f_BinarySearchTree0", JSConTest.tests.setVar("f_BinarySearchTree0", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  () {
      JSConTest.effects.propAss(this, "_root", null);
    }), []), (function BinarySearchTree () {
      this._root = null;
    }), false)), [], []);
  BinarySearchTree.prototype = {constructor : BinarySearchTree, add : JSConTest.tests.addContracts("f_add1", JSConTest.tests.setVar("f_add1", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (value) {
    var node = JSConTest.effects.fixObjectLiteral({value : value, left : null, right : null}),
          current;
      if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "_root")) === null))) {
          JSConTest.effects.propAss(this, "_root", node);
        }
        else {
          current = JSConTest.effects.propAcc(this, "_root");
          while (true) {
            if (JSConTest.effects.unbox((JSConTest.effects.unbox(value) < JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "value"))))) {
              if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "left")) === null))) {
                JSConTest.effects.propAss(current, "left", node);
                break ;
              }
              else {
                current = JSConTest.effects.propAcc(current, "left");
              };
            }
            else if (JSConTest.effects.unbox((JSConTest.effects.unbox(value) > JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "value"))))) {
              if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "right")) === null))) {
                JSConTest.effects.propAss(current, "right", node);
                break ;
              }
              else {
                current = JSConTest.effects.propAcc(current, "right");
              };
            }
            else {
              break ;
            };;
          }
        };
  }), ["value"]), ["c_2"], "f_add1"), (function add (value) {
    var node = {value : value, left : null, right : null},
          current;
      if ((this._root === null)) {
          this._root = node;
        }
        else {
          current = this._root;
          while (true) {
            if ((value < current.value)) {
              if ((current.left === null)) {
                current.left = node;
                break ;
              }
              else {
                current = current.left;
              };
            }
            else if ((value > current.value)) {
              if ((current.right === null)) {
                current.right = node;
                break ;
              }
              else {
                current = current.right;
              };
            }
            else {
              break ;
            };;
          }
        };
  }), true)), [{contract : JSConTest.tests.setVar("c_2", JSConTest.contracts.Method(bst, [JSConTest.contracts.Integer], JSConTest.contracts.Undefined, [], "f_add1")), count : 50. }], []), contains : JSConTest.tests.addContracts("f_contains3", JSConTest.tests.setVar("f_contains3", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (value) {
    var found = false,
          current = JSConTest.effects.propAcc(this, "_root");
      while (JSConTest.effects.unbox(( !found && current))) {
          if (JSConTest.effects.unbox((JSConTest.effects.unbox(value) < JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "value"))))) {
            current = JSConTest.effects.propAcc(current, "left");
          }
          else if (JSConTest.effects.unbox((JSConTest.effects.unbox(value) > JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "value"))))) {
            current = JSConTest.effects.propAcc(current, "right");
          }
          else {
            found = true;
          };;
        }
      return found;
  }), ["value"]), ["c_4"], "f_contains3"), (function contains (value) {
    var found = false,
          current = this._root;
      while (( !found && current)) {
          if ((value < current.value)) {
            current = current.left;
          }
          else if ((value > current.value)) {
            current = current.right;
          }
          else {
            found = true;
          };;
        }
      return found;
  }), true)), [{contract : JSConTest.tests.setVar("c_4", JSConTest.contracts.Method(bst, [JSConTest.contracts.Integer], JSConTest.contracts.Boolean, [], "f_contains3")), count : 50. }], []), remove : JSConTest.tests.addContracts("f_remove5", JSConTest.tests.setVar("f_remove5", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (value) {
    var found = false,
          parent = null,
          current = JSConTest.effects.propAcc(this, "_root"),
          childCount,
          replacement,
          replacementParent;
      while (JSConTest.effects.unbox(( !found && current))) {
          if (JSConTest.effects.unbox((JSConTest.effects.unbox(value) < JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "value"))))) {
            parent = current;
            current = JSConTest.effects.propAcc(current, "left");
          }
          else if (JSConTest.effects.unbox((JSConTest.effects.unbox(value) > JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "value"))))) {
            parent = current;
            current = JSConTest.effects.propAcc(current, "right");
          }
          else {
            found = true;
          };;
        }
      if (JSConTest.effects.unbox(found)) {
          childCount = (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "left")) !== null)? 1.  : 0. ) + JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "right")) !== null)? 1.  : 0. ));
          if (JSConTest.effects.unbox((JSConTest.effects.unbox(current) === JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "_root"))))) {
            switch (childCount) {case 0.  : JSConTest.effects.propAss(this, "_root", null);; break ;
            case 1.  : JSConTest.effects.propAss(this, "_root", (JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "right")) === null)? JSConTest.effects.propAcc(current, "left") : JSConTest.effects.propAcc(current, "right"));; break ;
            case 2.  : replacement = JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "_root"), "left");; while (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(replacement, "right")) !== null))) {
              replacementParent = replacement;
              replacement = JSConTest.effects.propAcc(replacement, "right");
            }; if (JSConTest.effects.unbox((JSConTest.effects.unbox(replacementParent) !== null))) {
              JSConTest.effects.propAss(replacementParent, "right", JSConTest.effects.propAcc(replacement, "left"));
              JSConTest.effects.propAss(replacement, "right", JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "_root"), "right"));
              JSConTest.effects.propAss(replacement, "left", JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "_root"), "left"));
            }
            else {
              JSConTest.effects.propAss(replacement, "right", JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "_root"), "right"));
            };; JSConTest.effects.propAss(this, "_root", replacement);
             }
          }
          else {
            switch (childCount) {case 0.  : if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "value")) < JSConTest.effects.unbox(JSConTest.effects.propAcc(parent, "value"))))) {
              JSConTest.effects.propAss(parent, "left", null);
            }
            else {
              JSConTest.effects.propAss(parent, "right", null);
            };; break ;
            case 1.  : if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "value")) < JSConTest.effects.unbox(JSConTest.effects.propAcc(parent, "value"))))) {
              JSConTest.effects.propAss(parent, "left", (JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "left")) === null)? JSConTest.effects.propAcc(current, "right") : JSConTest.effects.propAcc(current, "left"));
            }
            else {
              JSConTest.effects.propAss(parent, "right", (JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "left")) === null)? JSConTest.effects.propAcc(current, "right") : JSConTest.effects.propAcc(current, "left"));
            };; break ;
            case 2.  : replacement = JSConTest.effects.propAcc(current, "left");; replacementParent = current;; while (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(replacement, "right")) !== null))) {
              replacementParent = replacement;
              replacement = JSConTest.effects.propAcc(replacement, "right");
            }; JSConTest.effects.propAss(replacementParent, "right", JSConTest.effects.propAcc(replacement, "left"));; JSConTest.effects.propAss(replacement, "right", JSConTest.effects.propAcc(current, "right"));; JSConTest.effects.propAss(replacement, "left", JSConTest.effects.propAcc(current, "left"));; if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(current, "value")) < JSConTest.effects.unbox(JSConTest.effects.propAcc(parent, "value"))))) {
              JSConTest.effects.propAss(parent, "left", replacement);
            }
            else {
              JSConTest.effects.propAss(parent, "right", replacement);
            };
             }
          };
        };
  }), ["value"]), ["c_6"], "f_remove5"), (function remove (value) {
    var found = false,
          parent = null,
          current = this._root,
          childCount,
          replacement,
          replacementParent;
      while (( !found && current)) {
          if ((value < current.value)) {
            parent = current;
            current = current.left;
          }
          else if ((value > current.value)) {
            parent = current;
            current = current.right;
          }
          else {
            found = true;
          };;
        }
      if (found) {
          childCount = ((current.left !== null)? 1.  : 0.  + (current.right !== null)? 1.  : 0. );
          if ((current === this._root)) {
            switch (childCount) {case 0.  : this._root = null;; break ;
            case 1.  : this._root = (current.right === null)? current.left : current.right;; break ;
            case 2.  : replacement = this._root.left;; while ((replacement.right !== null)) {
              replacementParent = replacement;
              replacement = replacement.right;
            }; if ((replacementParent !== null)) {
              replacementParent.right = replacement.left;
              replacement.right = this._root.right;
              replacement.left = this._root.left;
            }
            else {
              replacement.right = this._root.right;
            };; this._root = replacement;
             }
          }
          else {
            switch (childCount) {case 0.  : if ((current.value < parent.value)) {
              parent.left = null;
            }
            else {
              parent.right = null;
            };; break ;
            case 1.  : if ((current.value < parent.value)) {
              parent.left = (current.left === null)? current.right : current.left;
            }
            else {
              parent.right = (current.left === null)? current.right : current.left;
            };; break ;
            case 2.  : replacement = current.left;; replacementParent = current;; while ((replacement.right !== null)) {
              replacementParent = replacement;
              replacement = replacement.right;
            }; replacementParent.right = replacement.left;; replacement.right = current.right;; replacement.left = current.left;; if ((current.value < parent.value)) {
              parent.left = replacement;
            }
            else {
              parent.right = replacement;
            };
             }
          };
        };
  }), true)), [{contract : JSConTest.tests.setVar("c_6", JSConTest.contracts.Method(bst, [JSConTest.contracts.Integer], JSConTest.contracts.Undefined, [], "f_remove5")), count : 50. }], []), size : (function size () {
    var length = 0. ;
      this.traverse((function  (node) {
        length++ ;
      }));
      return length;
  }), toArray : (function toArray () {
    var result = [];
      this.traverse((function  (node) {
        result.push(node.value);
      }));
      return result;
  }), toString : (function toString () {
    return this.toArray().toString();
  }), traverse : (function traverse (process) {
    var inOrder = JSConTest.tests.addContracts("f_inOrder7", JSConTest.tests.setVar("f_inOrder7", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  (node) {
          if (JSConTest.effects.unbox(node)) {
                if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(node, "left")) !== null))) {
                  JSConTest.effects.fCall(inOrder, [node.left]);
                };
                JSConTest.effects.mCall(process, "call", [this, node]);
                if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(node, "right")) !== null))) {
                  JSConTest.effects.fCall(inOrder, [node.right]);
                };
              };
        }), ["node"]), (function inOrder (node) {
          if (node) {
                if ((node.left !== null)) {
                  inOrder(node.left);
                };
                process.call(this, node);
                if ((node.right !== null)) {
                  inOrder(node.right);
                };
              };
        }), false)), [], []);
      inOrder(this._root);
  })};