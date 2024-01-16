var inherits = JSConTest.tests.addContracts("f_inherits1", JSConTest.tests.setVar("f_inherits1", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  (shuper, c) {
      var Inheriter = JSConTest.tests.addContracts("f_Inheriter0", JSConTest.tests.setVar("f_Inheriter0", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  () {
            
          }), []), (function Inheriter () {
            
          }), false)), [], []);
        JSConTest.effects.propAss(Inheriter, "prototype", JSConTest.effects.propAcc(shuper, "prototype"));
        JSConTest.effects.propAss(c, "prototype", JSConTest.effects.newCall(Inheriter, []));
        JSConTest.effects.propAss(c, "superConstructor", shuper);
    }), ["shuper", "c"]), (function inherits (shuper, c) {
      var Inheriter = JSConTest.tests.addContracts("f_Inheriter0", JSConTest.tests.setVar("f_Inheriter0", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  () {
            
          }), []), (function Inheriter () {
            
          }), false)), [], []);
        Inheriter.prototype = shuper.prototype;
        c.prototype = new Inheriter();
        c.superConstructor = shuper;
    }), false)), [], []);
  var OrderedCollection = JSConTest.tests.addContracts("f_OrderedCollection2", JSConTest.tests.setVar("f_OrderedCollection2", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  () {
      JSConTest.effects.propAss(this, "elms", JSConTest.effects.newCall(Array, []));
    }), []), (function OrderedCollection () {
      this.elms = new Array();
    }), false)), [], []);
  var Strength = JSConTest.tests.addContracts("f_Strength13", JSConTest.tests.setVar("f_Strength13", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  (strengthValue, name) {
      JSConTest.effects.propAss(this, "strengthValue", strengthValue);
        JSConTest.effects.propAss(this, "name", name);
    }), ["strengthValue", "name"]), (function Strength (strengthValue, name) {
      this.strengthValue = strengthValue;
        this.name = name;
    }), false)), [], []);
  var Constraint = JSConTest.tests.addContracts("f_Constraint22", JSConTest.tests.setVar("f_Constraint22", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  (strength) {
      JSConTest.effects.propAss(this, "strength", strength);
    }), ["strength"]), (function Constraint (strength) {
      this.strength = strength;
    }), false)), [], []);
  var UnaryConstraint = JSConTest.tests.addContracts("f_UnaryConstraint31", JSConTest.tests.setVar("f_UnaryConstraint31", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  (v, strength) {
      JSConTest.effects.mCall(JSConTest.effects.propAcc(UnaryConstraint, "superConstructor"), "call", [this, strength]);
        JSConTest.effects.propAss(this, "myOutput", v);
        JSConTest.effects.propAss(this, "satisfied", false);
        JSConTest.effects.mCall(this, "addConstraint", []);
    }), ["v", "strength"]), (function UnaryConstraint (v, strength) {
      UnaryConstraint.superConstructor.call(this, strength);
        this.myOutput = v;
        this.satisfied = false;
        this.addConstraint();
    }), false)), [], []);
  var StayConstraint = JSConTest.tests.addContracts("f_StayConstraint50", JSConTest.tests.setVar("f_StayConstraint50", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  (v, str) {
      JSConTest.effects.mCall(JSConTest.effects.propAcc(StayConstraint, "superConstructor"), "call", [this, v, str]);
    }), ["v", "str"]), (function StayConstraint (v, str) {
      StayConstraint.superConstructor.call(this, v, str);
    }), false)), [], []);
  var EditConstraint = JSConTest.tests.addContracts("f_EditConstraint53", JSConTest.tests.setVar("f_EditConstraint53", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  (v, str) {
      JSConTest.effects.mCall(JSConTest.effects.propAcc(EditConstraint, "superConstructor"), "call", [this, v, str]);
    }), ["v", "str"]), (function EditConstraint (v, str) {
      EditConstraint.superConstructor.call(this, v, str);
    }), false)), [], []);
  var BinaryConstraint = JSConTest.tests.addContracts("f_BinaryConstraint58", JSConTest.tests.setVar("f_BinaryConstraint58", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  (var1, var2, strength) {
      JSConTest.effects.mCall(JSConTest.effects.propAcc(BinaryConstraint, "superConstructor"), "call", [this, strength]);
        JSConTest.effects.propAss(this, "v1", var1);
        JSConTest.effects.propAss(this, "v2", var2);
        JSConTest.effects.propAss(this, "direction", JSConTest.effects.propAcc(Direction, "NONE"));
        JSConTest.effects.mCall(this, "addConstraint", []);
    }), ["var1", "var2", "strength"]), (function BinaryConstraint (var1, var2, strength) {
      BinaryConstraint.superConstructor.call(this, strength);
        this.v1 = var1;
        this.v2 = var2;
        this.direction = Direction.NONE;
        this.addConstraint();
    }), false)), [], []);
  var ScaleConstraint = JSConTest.tests.addContracts("f_ScaleConstraint79", JSConTest.tests.setVar("f_ScaleConstraint79", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  (src, scale, offset, dest, strength) {
      JSConTest.effects.propAss(this, "direction", JSConTest.effects.propAcc(Direction, "NONE"));
        JSConTest.effects.propAss(this, "scale", scale);
        JSConTest.effects.propAss(this, "offset", offset);
        JSConTest.effects.mCall(JSConTest.effects.propAcc(ScaleConstraint, "superConstructor"), "call", [this, src, dest, strength]);
    }), ["src", "scale", "offset", "dest", "strength"]), (function ScaleConstraint (src, scale, offset, dest, strength) {
      this.direction = Direction.NONE;
        this.scale = scale;
        this.offset = offset;
        ScaleConstraint.superConstructor.call(this, src, dest, strength);
    }), false)), [], []);
  var EqualityConstraint = JSConTest.tests.addContracts("f_EqualityConstraint90", JSConTest.tests.setVar("f_EqualityConstraint90", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  (var1, var2, strength) {
      JSConTest.effects.mCall(JSConTest.effects.propAcc(EqualityConstraint, "superConstructor"), "call", [this, var1, var2, strength]);
    }), ["var1", "var2", "strength"]), (function EqualityConstraint (var1, var2, strength) {
      EqualityConstraint.superConstructor.call(this, var1, var2, strength);
    }), false)), [], []);
  var Variable = JSConTest.tests.addContracts("f_Variable93", JSConTest.tests.setVar("f_Variable93", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  (name, initialValue) {
      JSConTest.effects.propAss(this, "value", (initialValue || 0. ));
        JSConTest.effects.propAss(this, "constraints", JSConTest.effects.newCall(OrderedCollection, []));
        JSConTest.effects.propAss(this, "determinedBy", null);
        JSConTest.effects.propAss(this, "mark", 0. );
        JSConTest.effects.propAss(this, "walkStrength", JSConTest.effects.propAcc(Strength, "WEAKEST"));
        JSConTest.effects.propAss(this, "stay", true);
        JSConTest.effects.propAss(this, "name", name);
    }), ["name", "initialValue"]), (function Variable (name, initialValue) {
      this.value = (initialValue || 0. );
        this.constraints = new OrderedCollection();
        this.determinedBy = null;
        this.mark = 0. ;
        this.walkStrength = Strength.WEAKEST;
        this.stay = true;
        this.name = name;
    }), false)), [], []);
  var Planner = JSConTest.tests.addContracts("f_Planner98", JSConTest.tests.setVar("f_Planner98", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  () {
      JSConTest.effects.propAss(this, "currentMark", 0. );
    }), []), (function Planner () {
      this.currentMark = 0. ;
    }), false)), [], []);
  var Plan = JSConTest.tests.addContracts("f_Plan115", JSConTest.tests.setVar("f_Plan115", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  () {
      JSConTest.effects.propAss(this, "v", JSConTest.effects.newCall(OrderedCollection, []));
    }), []), (function Plan () {
      this.v = new OrderedCollection();
    }), false)), [], []);
  var chainTest = JSConTest.tests.addContracts("f_chainTest124", JSConTest.tests.setVar("f_chainTest124", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (n) {
      planner = JSConTest.effects.newCall(Planner, []);
        var prev = null,
            first = null,
            last = null;
        for (var i = 0. ; JSConTest.effects.unbox((JSConTest.effects.unbox(i) <= JSConTest.effects.unbox(n))); i++ )  {
            var name = ("v" + JSConTest.effects.unbox(i));
            var v = JSConTest.effects.newCall(Variable, [name]);
            if (JSConTest.effects.unbox((JSConTest.effects.unbox(prev) != null))) JSConTest.effects.newCall(EqualityConstraint, [prev, v, JSConTest.effects.propAcc(Strength, "REQUIRED")]);;
            if (JSConTest.effects.unbox((JSConTest.effects.unbox(i) == 0. ))) first = v;;
            if (JSConTest.effects.unbox((JSConTest.effects.unbox(i) == JSConTest.effects.unbox(n)))) last = v;;
            prev = v;
          };
        JSConTest.effects.newCall(StayConstraint, [last, JSConTest.effects.propAcc(Strength, "STRONG_DEFAULT")]);
        var edit = JSConTest.effects.newCall(EditConstraint, [first, JSConTest.effects.propAcc(Strength, "PREFERRED")]);
        var edits = JSConTest.effects.newCall(OrderedCollection, []);
        JSConTest.effects.mCall(edits, "add", [edit]);
        var plan = JSConTest.effects.mCall(planner, "extractPlanFromConstraints", [edits]);
        for (var i = 0. ; JSConTest.effects.unbox((JSConTest.effects.unbox(i) < 100. )); i++ )  {
            JSConTest.effects.propAss(first, "value", i);
            JSConTest.effects.mCall(plan, "execute", []);
            if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(last, "value")) != JSConTest.effects.unbox(i)))) throw "Chain test failed.";
          };
    }), ["n"]), ["c_125"], "f_chainTest124"), (function chainTest (n) {
      planner = new Planner();
        var prev = null,
            first = null,
            last = null;
        for (var i = 0. ; (i <= n); i++ )  {
            var name = ("v" + i);
            var v = new Variable(name);
            if ((prev != null)) new EqualityConstraint(prev, v, Strength.REQUIRED);;
            if ((i == 0. )) first = v;;
            if ((i == n)) last = v;;
            prev = v;
          };
        new StayConstraint(last, Strength.STRONG_DEFAULT);
        var edit = new EditConstraint(first, Strength.PREFERRED);
        var edits = new OrderedCollection();
        edits.add(edit);
        var plan = planner.extractPlanFromConstraints(edits);
        for (var i = 0. ; (i < 100. ); i++ )  {
            first.value = i;
            plan.execute();
            if ((last.value != i)) throw "Chain test failed.";
          };
    }), true)), [], [JSConTest.tests.setVar("c_125", JSConTest.contracts.Function([JSConTest.contracts.Integer], JSConTest.contracts.Undefined, {pos : [], neg : [], fname : "f_chainTest124"}, "f_chainTest124"))]);
  var projectionTest = JSConTest.tests.addContracts("f_projectionTest126", JSConTest.tests.setVar("f_projectionTest126", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (n) {
      planner = JSConTest.effects.newCall(Planner, []);
        var scale = JSConTest.effects.newCall(Variable, ["scale", 10. ]);
        var offset = JSConTest.effects.newCall(Variable, ["offset", 1000. ]);
        var src = null,
            dst = null;
        var dests = JSConTest.effects.newCall(OrderedCollection, []);
        for (var i = 0. ; JSConTest.effects.unbox((JSConTest.effects.unbox(i) < JSConTest.effects.unbox(n))); i++ )  {
            src = JSConTest.effects.newCall(Variable, [("src" + JSConTest.effects.unbox(i)), i]);
            dst = JSConTest.effects.newCall(Variable, [("dst" + JSConTest.effects.unbox(i)), i]);
            JSConTest.effects.mCall(dests, "add", [dst]);
            JSConTest.effects.newCall(StayConstraint, [src, JSConTest.effects.propAcc(Strength, "NORMAL")]);
            JSConTest.effects.newCall(ScaleConstraint, [src, scale, offset, dst, JSConTest.effects.propAcc(Strength, "REQUIRED")]);
          };
        JSConTest.effects.fCall(change, [src, 17. ]);
        if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(dst, "value")) != 1170. ))) throw "Projection 1 failed";
        JSConTest.effects.fCall(change, [dst, 1050. ]);
        if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(src, "value")) != 5. ))) throw "Projection 2 failed";
        JSConTest.effects.fCall(change, [scale, 5. ]);
        for (var i = 0. ; JSConTest.effects.unbox((JSConTest.effects.unbox(i) < JSConTest.effects.unbox((JSConTest.effects.unbox(n) - 1. )))); i++ )  {
            if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.mCall(dests, "at", [i]), "value")) != JSConTest.effects.unbox((JSConTest.effects.unbox((JSConTest.effects.unbox(i) * 5. )) + 1000. ))))) throw "Projection 3 failed";
          };
        JSConTest.effects.fCall(change, [offset, 2000. ]);
        for (var i = 0. ; JSConTest.effects.unbox((JSConTest.effects.unbox(i) < JSConTest.effects.unbox((JSConTest.effects.unbox(n) - 1. )))); i++ )  {
            if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.mCall(dests, "at", [i]), "value")) != JSConTest.effects.unbox((JSConTest.effects.unbox((JSConTest.effects.unbox(i) * 5. )) + 2000. ))))) throw "Projection 4 failed";
          };
    }), ["n"]), ["c_127"], "f_projectionTest126"), (function projectionTest (n) {
      planner = new Planner();
        var scale = new Variable("scale", 10. );
        var offset = new Variable("offset", 1000. );
        var src = null,
            dst = null;
        var dests = new OrderedCollection();
        for (var i = 0. ; (i < n); i++ )  {
            src = new Variable(("src" + i), i);
            dst = new Variable(("dst" + i), i);
            dests.add(dst);
            new StayConstraint(src, Strength.NORMAL);
            new ScaleConstraint(src, scale, offset, dst, Strength.REQUIRED);
          };
        change(src, 17. );
        if ((dst.value != 1170. )) throw "Projection 1 failed";
        change(dst, 1050. );
        if ((src.value != 5. )) throw "Projection 2 failed";
        change(scale, 5. );
        for (var i = 0. ; (i < (n - 1. )); i++ )  {
            if ((dests.at(i).value != ((i * 5. ) + 1000. ))) throw "Projection 3 failed";
          };
        change(offset, 2000. );
        for (var i = 0. ; (i < (n - 1. )); i++ )  {
            if ((dests.at(i).value != ((i * 5. ) + 2000. ))) throw "Projection 4 failed";
          };
    }), true)), [], [JSConTest.tests.setVar("c_127", JSConTest.contracts.Function([JSConTest.contracts.Integer], JSConTest.contracts.Undefined, {pos : [], neg : [], fname : "f_projectionTest126"}, "f_projectionTest126"))]);
  var change = JSConTest.tests.addContracts("f_change128", JSConTest.tests.setVar("f_change128", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (v, newValue) {
      var edit = JSConTest.effects.newCall(EditConstraint, [v, JSConTest.effects.propAcc(Strength, "PREFERRED")]);
        var edits = JSConTest.effects.newCall(OrderedCollection, []);
        JSConTest.effects.mCall(edits, "add", [edit]);
        var plan = JSConTest.effects.mCall(planner, "extractPlanFromConstraints", [edits]);
        for (var i = 0. ; JSConTest.effects.unbox((JSConTest.effects.unbox(i) < 10. )); i++ )  {
            JSConTest.effects.propAss(v, "value", newValue);
            JSConTest.effects.mCall(plan, "execute", []);
          };
        JSConTest.effects.mCall(edit, "destroyConstraint", []);
    }), ["v", "newValue"]), ["c_129"], "f_change128"), (function change (v, newValue) {
      var edit = new EditConstraint(v, Strength.PREFERRED);
        var edits = new OrderedCollection();
        edits.add(edit);
        var plan = planner.extractPlanFromConstraints(edits);
        for (var i = 0. ; (i < 10. ); i++ )  {
            v.value = newValue;
            plan.execute();
          };
        edit.destroyConstraint();
    }), true)), [], [JSConTest.tests.setVar("c_129", JSConTest.contracts.Function([v, JSConTest.contracts.Integer], JSConTest.contracts.Undefined, {pos : [{type : 3. , property : "value", effect : {type : 2. , name : "v", fname : "f_change128"}}], neg : [], fname : "f_change128"}, "f_change128"))]);
  var deltaBlue = JSConTest.tests.addContracts("f_deltaBlue130", JSConTest.tests.setVar("f_deltaBlue130", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
      JSConTest.effects.fCall(chainTest, [100. ]);
        JSConTest.effects.fCall(projectionTest, [100. ]);
    }), []), ["c_131"], "f_deltaBlue130"), (function deltaBlue () {
      chainTest(100. );
        projectionTest(100. );
    }), true)), [{contract : JSConTest.tests.setVar("c_131", JSConTest.contracts.Function([JSConTest.contracts.Undefined], JSConTest.contracts.Undefined, {pos : [], neg : [], fname : "f_deltaBlue130"}, "f_deltaBlue130")), count : 1. }], []);
  OrderedCollection.prototype.add = JSConTest.tests.addContracts("f_OrderedCollection_prototype_add3", JSConTest.tests.setVar("f_OrderedCollection_prototype_add3", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (elm) {
    JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "elms"), "push", [elm]);
  }), ["elm"]), ["c_4"], "f_OrderedCollection_prototype_add3"), (function  (elm) {
    this.elms.push(elm);
  }), true)), [], [JSConTest.tests.setVar("c_4", JSConTest.contracts.Method(oc, [JSConTest.contracts.EObject([])], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "elms", effect : {type : 2. , name : "this", fname : "f_OrderedCollection_prototype_add3"}}}], neg : [], fname : "f_OrderedCollection_prototype_add3"}, "f_OrderedCollection_prototype_add3"))]);
  OrderedCollection.prototype.at = JSConTest.tests.addContracts("f_OrderedCollection_prototype_at5", JSConTest.tests.setVar("f_OrderedCollection_prototype_at5", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (index) {
    return JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "elms"), index);
  }), ["index"]), ["c_6"], "f_OrderedCollection_prototype_at5"), (function  (index) {
    return this.elms[index];
  }), true)), [], [JSConTest.tests.setVar("c_6", JSConTest.contracts.Method(oc, [JSConTest.contracts.Integer], JSConTest.contracts.Top, {pos : [{type : 7. , effect : {type : 4. , effect : {type : 3. , property : "elms", effect : {type : 2. , name : "this", fname : "f_OrderedCollection_prototype_at5"}}}}], neg : [], fname : "f_OrderedCollection_prototype_at5"}, "f_OrderedCollection_prototype_at5"))]);
  OrderedCollection.prototype.size = JSConTest.tests.addContracts("f_OrderedCollection_prototype_size7", JSConTest.tests.setVar("f_OrderedCollection_prototype_size7", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    return JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "elms"), "length");
  }), []), ["c_8"], "f_OrderedCollection_prototype_size7"), (function  () {
    return this.elms.length;
  }), true)), [], [JSConTest.tests.setVar("c_8", JSConTest.contracts.Method(oc, [], JSConTest.contracts.Integer, {pos : [{type : 7. , effect : {type : 3. , property : "length", effect : {type : 3. , property : "elms", effect : {type : 2. , name : "this", fname : "f_OrderedCollection_prototype_size7"}}}}], neg : [], fname : "f_OrderedCollection_prototype_size7"}, "f_OrderedCollection_prototype_size7"))]);
  OrderedCollection.prototype.removeFirst = JSConTest.tests.addContracts("f_OrderedCollection_prototype_removeFirst9", JSConTest.tests.setVar("f_OrderedCollection_prototype_removeFirst9", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    return JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "elms"), "pop", []);
  }), []), ["c_10"], "f_OrderedCollection_prototype_removeFirst9"), (function  () {
    return this.elms.pop();
  }), true)), [], [JSConTest.tests.setVar("c_10", JSConTest.contracts.Method(oc, [], JSConTest.contracts.EObject([]), {pos : [{type : 7. , effect : {type : 3. , property : "elms", effect : {type : 2. , name : "this", fname : "f_OrderedCollection_prototype_removeFirst9"}}}], neg : [], fname : "f_OrderedCollection_prototype_removeFirst9"}, "f_OrderedCollection_prototype_removeFirst9"))]);
  OrderedCollection.prototype.remove = JSConTest.tests.addContracts("f_OrderedCollection_prototype_remove11", JSConTest.tests.setVar("f_OrderedCollection_prototype_remove11", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (elm) {
    var index = 0. ,
          skipped = 0. ;
      for (var i = 0. ; JSConTest.effects.unbox((JSConTest.effects.unbox(i) < JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "elms"), "length")))); i++ )  {
          var value = JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "elms"), i);
          if (JSConTest.effects.unbox((JSConTest.effects.unbox(value) != JSConTest.effects.unbox(elm)))) {
            JSConTest.effects.propAss(JSConTest.effects.propAcc(this, "elms"), index, value);
            index++ ;
          }
          else {
            skipped++ ;
          };
        };
      for (var i = 0. ; JSConTest.effects.unbox((JSConTest.effects.unbox(i) < JSConTest.effects.unbox(skipped))); i++ )  JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "elms"), "pop", []);;
  }), ["elm"]), ["c_12"], "f_OrderedCollection_prototype_remove11"), (function  (elm) {
    var index = 0. ,
          skipped = 0. ;
      for (var i = 0. ; (i < this.elms.length); i++ )  {
          var value = this.elms[i];
          if ((value != elm)) {
            this.elms[index] = value;
            index++ ;
          }
          else {
            skipped++ ;
          };
        };
      for (var i = 0. ; (i < skipped); i++ )  this.elms.pop();;
  }), true)), [], [JSConTest.tests.setVar("c_12", JSConTest.contracts.Method(oc, [JSConTest.contracts.EObject([])], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "length", effect : {type : 3. , property : "elms", effect : {type : 2. , name : "this", fname : "f_OrderedCollection_prototype_remove11"}}}}, {type : 4. , effect : {type : 3. , property : "elms", effect : {type : 2. , name : "this", fname : "f_OrderedCollection_prototype_remove11"}}}], neg : [], fname : "f_OrderedCollection_prototype_remove11"}, "f_OrderedCollection_prototype_remove11"))]);
  Strength.stronger = JSConTest.tests.addContracts("f_Strength_stronger14", JSConTest.tests.setVar("f_Strength_stronger14", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (s1, s2) {
    return (JSConTest.effects.unbox(JSConTest.effects.propAcc(s1, "strengthValue")) < JSConTest.effects.unbox(JSConTest.effects.propAcc(s2, "strengthValue")));
  }), ["s1", "s2"]), ["c_15"], "f_Strength_stronger14"), (function  (s1, s2) {
    return (s1.strengthValue < s2.strengthValue);
  }), true)), [], [JSConTest.tests.setVar("c_15", JSConTest.contracts.Method(StrengthClass, [s, s], JSConTest.contracts.Boolean, {pos : [{type : 3. , property : "strengthValue", effect : {type : 2. , name : "s1", fname : "f_Strength_stronger14"}}, {type : 3. , property : "strengthValue", effect : {type : 2. , name : "s2", fname : "f_Strength_stronger14"}}], neg : [], fname : "f_Strength_stronger14"}, "f_Strength_stronger14"))]);
  Strength.weaker = JSConTest.tests.addContracts("f_Strength_weaker16", JSConTest.tests.setVar("f_Strength_weaker16", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  (s1, s2) {
    return (JSConTest.effects.unbox(JSConTest.effects.propAcc(s1, "strengthValue")) > JSConTest.effects.unbox(JSConTest.effects.propAcc(s2, "strengthValue")));
  }), ["s1", "s2"]), (function  (s1, s2) {
    return (s1.strengthValue > s2.strengthValue);
  }), false)), [], [JSConTest.tests.setVar("c_17", JSConTest.contracts.Method(StrengthClass, [s, s], JSConTest.contracts.Boolean, {pos : [], neg : [], fname : "f_Strength_weaker16"}, "f_Strength_weaker16"))]);
  Strength.weakestOf = JSConTest.tests.addContracts("f_Strength_weakestOf18", JSConTest.tests.setVar("f_Strength_weakestOf18", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  (s1, s2) {
    return JSConTest.effects.mCall(this, "weaker", [s1, s2])? s1 : s2;
  }), ["s1", "s2"]), (function  (s1, s2) {
    return this.weaker(s1, s2)? s1 : s2;
  }), false)), [], [JSConTest.tests.setVar("c_19", JSConTest.contracts.Method(StrengthClass, [s, s], s, {pos : [], neg : [], fname : "f_Strength_weakestOf18"}, "f_Strength_weakestOf18"))]);
  Strength.strongest = JSConTest.tests.addContracts("f_Strength_strongest20", JSConTest.tests.setVar("f_Strength_strongest20", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function  (s1, s2) {
    return JSConTest.effects.mCall(this, "stronger", [s1, s2])? s1 : s2;
  }), ["s1", "s2"]), (function  (s1, s2) {
    return this.stronger(s1, s2)? s1 : s2;
  }), false)), [], [JSConTest.tests.setVar("c_21", JSConTest.contracts.Method(StrengthClass, [s, s], s, {pos : [], neg : [], fname : "f_Strength_strongest20"}, "f_Strength_strongest20"))]);
  Strength.prototype.nextWeaker = (function  () {
    switch (this.strengthValue) {case 0.  : return Strength.WEAKEST;
        case 1.  : return Strength.WEAK_DEFAULT;
        case 2.  : return Strength.NORMAL;
        case 3.  : return Strength.STRONG_DEFAULT;
        case 4.  : return Strength.PREFERRED;
        case 5.  : return Strength.REQUIRED;
         }
  });
  Strength.REQUIRED = new Strength(0. , "required");
  Strength.STONG_PREFERRED = new Strength(1. , "strongPreferred");
  Strength.PREFERRED = new Strength(2. , "preferred");
  Strength.STRONG_DEFAULT = new Strength(3. , "strongDefault");
  Strength.NORMAL = new Strength(4. , "normal");
  Strength.WEAK_DEFAULT = new Strength(5. , "weakDefault");
  Strength.WEAKEST = new Strength(6. , "weakest");
  Constraint.prototype.addConstraint = JSConTest.tests.addContracts("f_Constraint_prototype_addConstraint23", JSConTest.tests.setVar("f_Constraint_prototype_addConstraint23", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    JSConTest.effects.mCall(this, "addToGraph", []);
      JSConTest.effects.mCall(planner, "incrementalAdd", [this]);
  }), []), ["c_24"], "f_Constraint_prototype_addConstraint23"), (function  () {
    this.addToGraph();
      planner.incrementalAdd(this);
  }), true)), [], [JSConTest.tests.setVar("c_24", JSConTest.contracts.Method(c, [], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 3. , property : "v1", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_addConstraint23"}}}}}, {type : 7. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 3. , property : "v2", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_addConstraint23"}}}}}, {type : 7. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 3. , property : "myOutput", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_addConstraint23"}}}}}, {type : 7. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 3. , property : "scale", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_addConstraint23"}}}}}, {type : 7. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 3. , property : "offset", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_addConstraint23"}}}}}, {type : 3. , property : "direction", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_addConstraint23"}}, {type : 3. , property : "satisfied", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_addConstraint23"}}], neg : [], fname : "f_Constraint_prototype_addConstraint23"}, "f_Constraint_prototype_addConstraint23"))]);
  Constraint.prototype.satisfy = JSConTest.tests.addContracts("f_Constraint_prototype_satisfy25", JSConTest.tests.setVar("f_Constraint_prototype_satisfy25", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (mark) {
    JSConTest.effects.mCall(this, "chooseMethod", [mark]);
      if (JSConTest.effects.unbox( !JSConTest.effects.mCall(this, "isSatisfied", []))) {
          if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "strength")) == JSConTest.effects.unbox(JSConTest.effects.propAcc(Strength, "REQUIRED"))))) throw "Could not satisfy a required constraint!";
          return null;
        };
      JSConTest.effects.mCall(this, "markInputs", [mark]);
      var out = JSConTest.effects.mCall(this, "output", []);
      var overridden = JSConTest.effects.propAcc(out, "determinedBy");
      if (JSConTest.effects.unbox((JSConTest.effects.unbox(overridden) != null))) JSConTest.effects.mCall(overridden, "markUnsatisfied", []);;
      JSConTest.effects.propAss(out, "determinedBy", this);
      if (JSConTest.effects.unbox( !JSConTest.effects.mCall(planner, "addPropagate", [this, mark]))) throw "Cycle encountered";
      JSConTest.effects.propAss(out, "mark", mark);
      return overridden;
  }), ["mark"]), ["c_26"], "f_Constraint_prototype_satisfy25"), (function  (mark) {
    this.chooseMethod(mark);
      if ( !this.isSatisfied()) {
          if ((this.strength == Strength.REQUIRED)) throw "Could not satisfy a required constraint!";
          return null;
        };
      this.markInputs(mark);
      var out = this.output();
      var overridden = out.determinedBy;
      if ((overridden != null)) overridden.markUnsatisfied();;
      out.determinedBy = this;
      if ( !planner.addPropagate(this, mark)) throw "Cycle encountered";
      out.mark = mark;
      return overridden;
  }), true)), [], [JSConTest.tests.setVar("c_26", JSConTest.contracts.Method(c, [JSConTest.contracts.Integer], nullc, {pos : [{type : 7. , effect : {type : 3. , property : "mark", effect : {type : 3. , property : "v1", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_satisfy25"}}}}, {type : 7. , effect : {type : 3. , property : "walkStrength", effect : {type : 3. , property : "v1", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_satisfy25"}}}}, {type : 7. , effect : {type : 3. , property : "mark", effect : {type : 3. , property : "v2", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_satisfy25"}}}}, {type : 7. , effect : {type : 3. , property : "walkStrength", effect : {type : 3. , property : "v2", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_satisfy25"}}}}, {type : 7. , effect : {type : 3. , property : "strength", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_satisfy25"}}}, {type : 7. , effect : {type : 3. , property : "mark", effect : {type : 3. , property : "myOutput", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_satisfy25"}}}}, {type : 7. , effect : {type : 3. , property : "walkStrength", effect : {type : 3. , property : "myOutput", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_satisfy25"}}}}, {type : 3. , property : "direction", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_satisfy25"}}, {type : 3. , property : "satisfied", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_satisfy25"}}, {type : 3. , property : "mark", effect : {type : 3. , property : "offset", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_satisfy25"}}}, {type : 3. , property : "mark", effect : {type : 3. , property : "scale", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_satisfy25"}}}], neg : [], fname : "f_Constraint_prototype_satisfy25"}, "f_Constraint_prototype_satisfy25"))]);
  Constraint.prototype.destroyConstraint = JSConTest.tests.addContracts("f_Constraint_prototype_destroyConstraint27", JSConTest.tests.setVar("f_Constraint_prototype_destroyConstraint27", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    if (JSConTest.effects.unbox(JSConTest.effects.mCall(this, "isSatisfied", []))) JSConTest.effects.mCall(planner, "incrementalRemove", [this]);
        else JSConTest.effects.mCall(this, "removeFromGraph", []);;
  }), []), ["c_28"], "f_Constraint_prototype_destroyConstraint27"), (function  () {
    if (this.isSatisfied()) planner.incrementalRemove(this);
        else this.removeFromGraph();;
  }), true)), [], [JSConTest.tests.setVar("c_28", JSConTest.contracts.Method(c, [], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "satisfied", effect : {type : 2. , name : "this", fname : "f_Constraint_prototype_destroyConstraint27"}}}], neg : [], fname : "f_Constraint_prototype_destroyConstraint27"}, "f_Constraint_prototype_destroyConstraint27"))]);
  Constraint.prototype.isInput = JSConTest.tests.addContracts("f_Constraint_prototype_isInput29", JSConTest.tests.setVar("f_Constraint_prototype_isInput29", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    return false;
  }), []), ["c_30"], "f_Constraint_prototype_isInput29"), (function  () {
    return false;
  }), true)), [], [JSConTest.tests.setVar("c_30", JSConTest.contracts.Method(c, [], JSConTest.contracts.Boolean, {pos : [], neg : [], fname : "f_Constraint_prototype_isInput29"}, "f_Constraint_prototype_isInput29"))]);
  inherits(Constraint, UnaryConstraint);
  UnaryConstraint.prototype.addToGraph = JSConTest.tests.addContracts("f_UnaryConstraint_prototype_addToGraph32", JSConTest.tests.setVar("f_UnaryConstraint_prototype_addToGraph32", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "myOutput"), "addConstraint", [this]);
      JSConTest.effects.propAss(this, "satisfied", false);
  }), []), ["c_33"], "f_UnaryConstraint_prototype_addToGraph32"), (function  () {
    this.myOutput.addConstraint(this);
      this.satisfied = false;
  }), true)), [], [JSConTest.tests.setVar("c_33", JSConTest.contracts.Method(uc, [], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 3. , property : "myOutput", effect : {type : 2. , name : "this", fname : "f_UnaryConstraint_prototype_addToGraph32"}}}}}, {type : 3. , property : "satisfied", effect : {type : 2. , name : "this", fname : "f_UnaryConstraint_prototype_addToGraph32"}}], neg : [], fname : "f_UnaryConstraint_prototype_addToGraph32"}, "f_UnaryConstraint_prototype_addToGraph32"))]);
  UnaryConstraint.prototype.chooseMethod = JSConTest.tests.addContracts("f_UnaryConstraint_prototype_chooseMethod34", JSConTest.tests.setVar("f_UnaryConstraint_prototype_chooseMethod34", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (mark) {
    JSConTest.effects.propAss(this, "satisfied", ((JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "myOutput"), "mark")) != JSConTest.effects.unbox(mark)) && JSConTest.effects.mCall(Strength, "stronger", [JSConTest.effects.propAcc(this, "strength"), JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "myOutput"), "walkStrength")])));
  }), ["mark"]), ["c_35"], "f_UnaryConstraint_prototype_chooseMethod34"), (function  (mark) {
    this.satisfied = ((this.myOutput.mark != mark) && Strength.stronger(this.strength, this.myOutput.walkStrength));
  }), true)), [], [JSConTest.tests.setVar("c_35", JSConTest.contracts.Method(uc, [JSConTest.contracts.Integer], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "mark", effect : {type : 3. , property : "myOutput", effect : {type : 2. , name : "this", fname : "f_UnaryConstraint_prototype_chooseMethod34"}}}}, {type : 7. , effect : {type : 3. , property : "walkStrength", effect : {type : 3. , property : "myOutput", effect : {type : 2. , name : "this", fname : "f_UnaryConstraint_prototype_chooseMethod34"}}}}, {type : 7. , effect : {type : 3. , property : "strength", effect : {type : 2. , name : "this", fname : "f_UnaryConstraint_prototype_chooseMethod34"}}}, {type : 3. , property : "satisfied", effect : {type : 2. , name : "this", fname : "f_UnaryConstraint_prototype_chooseMethod34"}}], neg : [], fname : "f_UnaryConstraint_prototype_chooseMethod34"}, "f_UnaryConstraint_prototype_chooseMethod34"))]);
  UnaryConstraint.prototype.isSatisfied = JSConTest.tests.addContracts("f_UnaryConstraint_prototype_isSatisfied36", JSConTest.tests.setVar("f_UnaryConstraint_prototype_isSatisfied36", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    return JSConTest.effects.propAcc(this, "satisfied");
  }), []), ["c_37"], "f_UnaryConstraint_prototype_isSatisfied36"), (function  () {
    return this.satisfied;
  }), true)), [], [JSConTest.tests.setVar("c_37", JSConTest.contracts.Method(uc, [], JSConTest.contracts.Boolean, {pos : [{type : 7. , effect : {type : 3. , property : "satisfied", effect : {type : 2. , name : "this", fname : "f_UnaryConstraint_prototype_isSatisfied36"}}}], neg : [], fname : "f_UnaryConstraint_prototype_isSatisfied36"}, "f_UnaryConstraint_prototype_isSatisfied36"))]);
  UnaryConstraint.prototype.markInputs = JSConTest.tests.addContracts("f_UnaryConstraint_prototype_markInputs38", JSConTest.tests.setVar("f_UnaryConstraint_prototype_markInputs38", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (mark) {
    return ;
  }), ["mark"]), ["c_39"], "f_UnaryConstraint_prototype_markInputs38"), (function  (mark) {
    return ;
  }), true)), [], [JSConTest.tests.setVar("c_39", JSConTest.contracts.Method(uc, [JSConTest.contracts.Integer], JSConTest.contracts.Undefined, {pos : [], neg : [], fname : "f_UnaryConstraint_prototype_markInputs38"}, "f_UnaryConstraint_prototype_markInputs38"))]);
  UnaryConstraint.prototype.output = JSConTest.tests.addContracts("f_UnaryConstraint_prototype_output40", JSConTest.tests.setVar("f_UnaryConstraint_prototype_output40", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    return JSConTest.effects.propAcc(this, "myOutput");
  }), []), ["c_41"], "f_UnaryConstraint_prototype_output40"), (function  () {
    return this.myOutput;
  }), true)), [], [JSConTest.tests.setVar("c_41", JSConTest.contracts.Method(uc, [], v, {pos : [{type : 7. , effect : {type : 3. , property : "myOutput", effect : {type : 2. , name : "this", fname : "f_UnaryConstraint_prototype_output40"}}}], neg : [], fname : "f_UnaryConstraint_prototype_output40"}, "f_UnaryConstraint_prototype_output40"))]);
  UnaryConstraint.prototype.recalculate = JSConTest.tests.addContracts("f_UnaryConstraint_prototype_recalculate42", JSConTest.tests.setVar("f_UnaryConstraint_prototype_recalculate42", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    JSConTest.effects.propAss(JSConTest.effects.propAcc(this, "myOutput"), "walkStrength", JSConTest.effects.propAcc(this, "strength"));
      JSConTest.effects.propAss(JSConTest.effects.propAcc(this, "myOutput"), "stay",  !JSConTest.effects.mCall(this, "isInput", []));
      if (JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "myOutput"), "stay"))) JSConTest.effects.mCall(this, "execute", []);;
  }), []), ["c_43"], "f_UnaryConstraint_prototype_recalculate42"), (function  () {
    this.myOutput.walkStrength = this.strength;
      this.myOutput.stay =  !this.isInput();
      if (this.myOutput.stay) this.execute();;
  }), true)), [], [JSConTest.tests.setVar("c_43", JSConTest.contracts.Method(uc, [], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "strength", effect : {type : 2. , name : "this", fname : "f_UnaryConstraint_prototype_recalculate42"}}}, {type : 3. , property : "walkStrength", effect : {type : 3. , property : "myOutput", effect : {type : 2. , name : "this", fname : "f_UnaryConstraint_prototype_recalculate42"}}}, {type : 3. , property : "stay", effect : {type : 3. , property : "myOutput", effect : {type : 2. , name : "this", fname : "f_UnaryConstraint_prototype_recalculate42"}}}], neg : [], fname : "f_UnaryConstraint_prototype_recalculate42"}, "f_UnaryConstraint_prototype_recalculate42"))]);
  UnaryConstraint.prototype.markUnsatisfied = JSConTest.tests.addContracts("f_UnaryConstraint_prototype_markUnsatisfied44", JSConTest.tests.setVar("f_UnaryConstraint_prototype_markUnsatisfied44", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    JSConTest.effects.propAss(this, "satisfied", false);
  }), []), ["c_45"], "f_UnaryConstraint_prototype_markUnsatisfied44"), (function  () {
    this.satisfied = false;
  }), true)), [], [JSConTest.tests.setVar("c_45", JSConTest.contracts.Method(uc, [], JSConTest.contracts.Undefined, {pos : [{type : 3. , property : "satisfied", effect : {type : 2. , name : "this", fname : "f_UnaryConstraint_prototype_markUnsatisfied44"}}], neg : [], fname : "f_UnaryConstraint_prototype_markUnsatisfied44"}, "f_UnaryConstraint_prototype_markUnsatisfied44"))]);
  UnaryConstraint.prototype.inputsKnown = JSConTest.tests.addContracts("f_UnaryConstraint_prototype_inputsKnown46", JSConTest.tests.setVar("f_UnaryConstraint_prototype_inputsKnown46", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    return true;
  }), []), ["c_47"], "f_UnaryConstraint_prototype_inputsKnown46"), (function  () {
    return true;
  }), true)), [], [JSConTest.tests.setVar("c_47", JSConTest.contracts.Method(uc, [], JSConTest.contracts.Boolean, {pos : [], neg : [], fname : "f_UnaryConstraint_prototype_inputsKnown46"}, "f_UnaryConstraint_prototype_inputsKnown46"))]);
  UnaryConstraint.prototype.removeFromGraph = JSConTest.tests.addContracts("f_UnaryConstraint_prototype_removeFromGraph48", JSConTest.tests.setVar("f_UnaryConstraint_prototype_removeFromGraph48", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "myOutput")) != null))) JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "myOutput"), "removeConstraint", [this]);;
      JSConTest.effects.propAss(this, "satisfied", false);
  }), []), ["c_49"], "f_UnaryConstraint_prototype_removeFromGraph48"), (function  () {
    if ((this.myOutput != null)) this.myOutput.removeConstraint(this);;
      this.satisfied = false;
  }), true)), [], [JSConTest.tests.setVar("c_49", JSConTest.contracts.Method(uc, [], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "length", effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 3. , property : "myOutput", effect : {type : 2. , name : "this", fname : "f_UnaryConstraint_prototype_removeFromGraph48"}}}}}}, {type : 4. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 3. , property : "myOutput", effect : {type : 2. , name : "this", fname : "f_UnaryConstraint_prototype_removeFromGraph48"}}}}}, {type : 3. , property : "determinedBy", effect : {type : 3. , property : "myOutput", effect : {type : 2. , name : "this", fname : "f_UnaryConstraint_prototype_removeFromGraph48"}}}, {type : 3. , property : "satisfied", effect : {type : 2. , name : "this", fname : "f_UnaryConstraint_prototype_removeFromGraph48"}}], neg : [], fname : "f_UnaryConstraint_prototype_removeFromGraph48"}, "f_UnaryConstraint_prototype_removeFromGraph48"))]);
  inherits(UnaryConstraint, StayConstraint);
  StayConstraint.prototype.execute = JSConTest.tests.addContracts("f_StayConstraint_prototype_execute51", JSConTest.tests.setVar("f_StayConstraint_prototype_execute51", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    return ;
  }), []), ["c_52"], "f_StayConstraint_prototype_execute51"), (function  () {
    return ;
  }), true)), [], [JSConTest.tests.setVar("c_52", JSConTest.contracts.Method(stc, [], JSConTest.contracts.Undefined, {pos : [], neg : [], fname : "f_StayConstraint_prototype_execute51"}, "f_StayConstraint_prototype_execute51"))]);
  inherits(UnaryConstraint, EditConstraint);
  EditConstraint.prototype.isInput = JSConTest.tests.addContracts("f_EditConstraint_prototype_isInput54", JSConTest.tests.setVar("f_EditConstraint_prototype_isInput54", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    return true;
  }), []), ["c_55"], "f_EditConstraint_prototype_isInput54"), (function  () {
    return true;
  }), true)), [], [JSConTest.tests.setVar("c_55", JSConTest.contracts.Method(edc, [], JSConTest.contracts.Boolean, {pos : [], neg : [], fname : "f_EditConstraint_prototype_isInput54"}, "f_EditConstraint_prototype_isInput54"))]);
  EditConstraint.prototype.execute = JSConTest.tests.addContracts("f_EditConstraint_prototype_execute56", JSConTest.tests.setVar("f_EditConstraint_prototype_execute56", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    return ;
  }), []), ["c_57"], "f_EditConstraint_prototype_execute56"), (function  () {
    return ;
  }), true)), [], [JSConTest.tests.setVar("c_57", JSConTest.contracts.Method(edc, [], JSConTest.contracts.Undefined, {pos : [], neg : [], fname : "f_EditConstraint_prototype_execute56"}, "f_EditConstraint_prototype_execute56"))]);
  var Direction = new Object();
  Direction.NONE = 0. ;
  Direction.FORWARD = 1. ;
  Direction.BACKWARD =  -1. ;
  inherits(Constraint, BinaryConstraint);
  BinaryConstraint.prototype.chooseMethod = JSConTest.tests.addContracts("f_BinaryConstraint_prototype_chooseMethod59", JSConTest.tests.setVar("f_BinaryConstraint_prototype_chooseMethod59", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (mark) {
    if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "v1"), "mark")) == JSConTest.effects.unbox(mark)))) {
          JSConTest.effects.propAss(this, "direction", ((JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "v1"), "mark")) != JSConTest.effects.unbox(mark)) && JSConTest.effects.mCall(Strength, "stronger", [JSConTest.effects.propAcc(this, "strength"), JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "v2"), "walkStrength")]))? JSConTest.effects.propAcc(Direction, "FORWARD") : JSConTest.effects.propAcc(Direction, "NONE"));
        };
      if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "v2"), "mark")) == JSConTest.effects.unbox(mark)))) {
          JSConTest.effects.propAss(this, "direction", ((JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "v1"), "mark")) != JSConTest.effects.unbox(mark)) && JSConTest.effects.mCall(Strength, "stronger", [JSConTest.effects.propAcc(this, "strength"), JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "v1"), "walkStrength")]))? JSConTest.effects.propAcc(Direction, "BACKWARD") : JSConTest.effects.propAcc(Direction, "NONE"));
        };
      if (JSConTest.effects.unbox(JSConTest.effects.mCall(Strength, "weaker", [JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "v1"), "walkStrength"), JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "v2"), "walkStrength")]))) {
          JSConTest.effects.propAss(this, "direction", JSConTest.effects.mCall(Strength, "stronger", [JSConTest.effects.propAcc(this, "strength"), JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "v1"), "walkStrength")])? JSConTest.effects.propAcc(Direction, "BACKWARD") : JSConTest.effects.propAcc(Direction, "NONE"));
        }
        else {
          JSConTest.effects.propAss(this, "direction", JSConTest.effects.mCall(Strength, "stronger", [JSConTest.effects.propAcc(this, "strength"), JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "v2"), "walkStrength")])? JSConTest.effects.propAcc(Direction, "FORWARD") : JSConTest.effects.propAcc(Direction, "BACKWARD"));
        };
  }), ["mark"]), ["c_60"], "f_BinaryConstraint_prototype_chooseMethod59"), (function  (mark) {
    if ((this.v1.mark == mark)) {
          this.direction = ((this.v1.mark != mark) && Strength.stronger(this.strength, this.v2.walkStrength))? Direction.FORWARD : Direction.NONE;
        };
      if ((this.v2.mark == mark)) {
          this.direction = ((this.v1.mark != mark) && Strength.stronger(this.strength, this.v1.walkStrength))? Direction.BACKWARD : Direction.NONE;
        };
      if (Strength.weaker(this.v1.walkStrength, this.v2.walkStrength)) {
          this.direction = Strength.stronger(this.strength, this.v1.walkStrength)? Direction.BACKWARD : Direction.NONE;
        }
        else {
          this.direction = Strength.stronger(this.strength, this.v2.walkStrength)? Direction.FORWARD : Direction.BACKWARD;
        };
  }), true)), [], [JSConTest.tests.setVar("c_60", JSConTest.contracts.Method(bc, [JSConTest.contracts.Integer], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "mark", effect : {type : 3. , property : "v1", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_chooseMethod59"}}}}, {type : 7. , effect : {type : 3. , property : "walkStrength", effect : {type : 3. , property : "v1", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_chooseMethod59"}}}}, {type : 7. , effect : {type : 3. , property : "mark", effect : {type : 3. , property : "v2", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_chooseMethod59"}}}}, {type : 7. , effect : {type : 3. , property : "walkStrength", effect : {type : 3. , property : "v2", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_chooseMethod59"}}}}, {type : 7. , effect : {type : 3. , property : "strength", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_chooseMethod59"}}}, {type : 3. , property : "direction", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_chooseMethod59"}}], neg : [], fname : "f_BinaryConstraint_prototype_chooseMethod59"}, "f_BinaryConstraint_prototype_chooseMethod59"))]);
  BinaryConstraint.prototype.addToGraph = JSConTest.tests.addContracts("f_BinaryConstraint_prototype_addToGraph61", JSConTest.tests.setVar("f_BinaryConstraint_prototype_addToGraph61", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "v1"), "addConstraint", [this]);
      JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "v2"), "addConstraint", [this]);
      JSConTest.effects.propAss(this, "direction", JSConTest.effects.propAcc(Direction, "NONE"));
  }), []), ["c_62"], "f_BinaryConstraint_prototype_addToGraph61"), (function  () {
    this.v1.addConstraint(this);
      this.v2.addConstraint(this);
      this.direction = Direction.NONE;
  }), true)), [], [JSConTest.tests.setVar("c_62", JSConTest.contracts.Method(bc, [], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 3. , property : "v1", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_addToGraph61"}}}}}, {type : 7. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 3. , property : "v2", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_addToGraph61"}}}}}, {type : 3. , property : "direction", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_addToGraph61"}}], neg : [], fname : "f_BinaryConstraint_prototype_addToGraph61"}, "f_BinaryConstraint_prototype_addToGraph61"))]);
  BinaryConstraint.prototype.isSatisfied = JSConTest.tests.addContracts("f_BinaryConstraint_prototype_isSatisfied63", JSConTest.tests.setVar("f_BinaryConstraint_prototype_isSatisfied63", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    return (JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "direction")) != JSConTest.effects.unbox(JSConTest.effects.propAcc(Direction, "NONE")));
  }), []), ["c_64"], "f_BinaryConstraint_prototype_isSatisfied63"), (function  () {
    return (this.direction != Direction.NONE);
  }), true)), [], [JSConTest.tests.setVar("c_64", JSConTest.contracts.Method(bc, [], JSConTest.contracts.Boolean, {pos : [{type : 7. , effect : {type : 3. , property : "direction", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_isSatisfied63"}}}], neg : [], fname : "f_BinaryConstraint_prototype_isSatisfied63"}, "f_BinaryConstraint_prototype_isSatisfied63"))]);
  BinaryConstraint.prototype.markInputs = JSConTest.tests.addContracts("f_BinaryConstraint_prototype_markInputs65", JSConTest.tests.setVar("f_BinaryConstraint_prototype_markInputs65", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (mark) {
    JSConTest.effects.propAss(JSConTest.effects.mCall(this, "input", []), "mark", mark);
  }), ["mark"]), ["c_66"], "f_BinaryConstraint_prototype_markInputs65"), (function  (mark) {
    this.input().mark = mark;
  }), true)), [], [JSConTest.tests.setVar("c_66", JSConTest.contracts.Method(bc, [JSConTest.contracts.Integer], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "direction", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_markInputs65"}}}, {type : 7. , effect : {type : 3. , property : "v1", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_markInputs65"}}}, {type : 7. , effect : {type : 3. , property : "v2", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_markInputs65"}}}], neg : [], fname : "f_BinaryConstraint_prototype_markInputs65"}, "f_BinaryConstraint_prototype_markInputs65"))]);
  BinaryConstraint.prototype.input = JSConTest.tests.addContracts("f_BinaryConstraint_prototype_input67", JSConTest.tests.setVar("f_BinaryConstraint_prototype_input67", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    return (JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "direction")) == JSConTest.effects.unbox(JSConTest.effects.propAcc(Direction, "FORWARD")))? JSConTest.effects.propAcc(this, "v1") : JSConTest.effects.propAcc(this, "v2");
  }), []), ["c_68"], "f_BinaryConstraint_prototype_input67"), (function  () {
    return (this.direction == Direction.FORWARD)? this.v1 : this.v2;
  }), true)), [], [JSConTest.tests.setVar("c_68", JSConTest.contracts.Method(bc, [], v, {pos : [{type : 7. , effect : {type : 3. , property : "direction", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_input67"}}}, {type : 7. , effect : {type : 3. , property : "v1", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_input67"}}}, {type : 7. , effect : {type : 3. , property : "v2", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_input67"}}}], neg : [], fname : "f_BinaryConstraint_prototype_input67"}, "f_BinaryConstraint_prototype_input67"))]);
  BinaryConstraint.prototype.output = JSConTest.tests.addContracts("f_BinaryConstraint_prototype_output69", JSConTest.tests.setVar("f_BinaryConstraint_prototype_output69", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    return (JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "direction")) == JSConTest.effects.unbox(JSConTest.effects.propAcc(Direction, "FORWARD")))? JSConTest.effects.propAcc(this, "v2") : JSConTest.effects.propAcc(this, "v1");
  }), []), ["c_70"], "f_BinaryConstraint_prototype_output69"), (function  () {
    return (this.direction == Direction.FORWARD)? this.v2 : this.v1;
  }), true)), [], [JSConTest.tests.setVar("c_70", JSConTest.contracts.Method(bc, [], v, {pos : [{type : 7. , effect : {type : 3. , property : "direction", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_output69"}}}, {type : 7. , effect : {type : 3. , property : "v2", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_output69"}}}, {type : 7. , effect : {type : 3. , property : "v1", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_output69"}}}], neg : [], fname : "f_BinaryConstraint_prototype_output69"}, "f_BinaryConstraint_prototype_output69"))]);
  BinaryConstraint.prototype.recalculate = JSConTest.tests.addContracts("f_BinaryConstraint_prototype_recalculate71", JSConTest.tests.setVar("f_BinaryConstraint_prototype_recalculate71", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    var ihn = JSConTest.effects.mCall(this, "input", []),
          out = JSConTest.effects.mCall(this, "output", []);
      JSConTest.effects.propAss(out, "walkStrength", JSConTest.effects.mCall(Strength, "weakestOf", [JSConTest.effects.propAcc(this, "strength"), JSConTest.effects.propAcc(ihn, "walkStrength")]));
      JSConTest.effects.propAss(out, "stay", JSConTest.effects.propAcc(ihn, "stay"));
      if (JSConTest.effects.unbox(JSConTest.effects.propAcc(out, "stay"))) JSConTest.effects.mCall(this, "execute", []);;
  }), []), ["c_72"], "f_BinaryConstraint_prototype_recalculate71"), (function  () {
    var ihn = this.input(),
          out = this.output();
      out.walkStrength = Strength.weakestOf(this.strength, ihn.walkStrength);
      out.stay = ihn.stay;
      if (out.stay) this.execute();;
  }), true)), [], [JSConTest.tests.setVar("c_72", JSConTest.contracts.Method(bc, [], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "direction", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_recalculate71"}}}, {type : 7. , effect : {type : 3. , property : "v1", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_recalculate71"}}}, {type : 7. , effect : {type : 3. , property : "v2", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_recalculate71"}}}, {type : 7. , effect : {type : 3. , property : "strength", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_recalculate71"}}}], neg : [], fname : "f_BinaryConstraint_prototype_recalculate71"}, "f_BinaryConstraint_prototype_recalculate71"))]);
  BinaryConstraint.prototype.markUnsatisfied = JSConTest.tests.addContracts("f_BinaryConstraint_prototype_markUnsatisfied73", JSConTest.tests.setVar("f_BinaryConstraint_prototype_markUnsatisfied73", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    JSConTest.effects.propAss(this, "direction", JSConTest.effects.propAcc(Direction, "NONE"));
  }), []), ["c_74"], "f_BinaryConstraint_prototype_markUnsatisfied73"), (function  () {
    this.direction = Direction.NONE;
  }), true)), [], [JSConTest.tests.setVar("c_74", JSConTest.contracts.Method(bc, [], JSConTest.contracts.Undefined, {pos : [{type : 3. , property : "direction", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_markUnsatisfied73"}}], neg : [], fname : "f_BinaryConstraint_prototype_markUnsatisfied73"}, "f_BinaryConstraint_prototype_markUnsatisfied73"))]);
  BinaryConstraint.prototype.inputsKnown = JSConTest.tests.addContracts("f_BinaryConstraint_prototype_inputsKnown75", JSConTest.tests.setVar("f_BinaryConstraint_prototype_inputsKnown75", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (mark) {
    var i = JSConTest.effects.mCall(this, "input", []);
      return (((JSConTest.effects.unbox(JSConTest.effects.propAcc(i, "mark")) == JSConTest.effects.unbox(mark)) || JSConTest.effects.propAcc(i, "stay")) || (JSConTest.effects.unbox(JSConTest.effects.propAcc(i, "determinedBy")) == null));
  }), ["mark"]), ["c_76"], "f_BinaryConstraint_prototype_inputsKnown75"), (function  (mark) {
    var i = this.input();
      return (((i.mark == mark) || i.stay) || (i.determinedBy == null));
  }), true)), [], [JSConTest.tests.setVar("c_76", JSConTest.contracts.Method(bc, [JSConTest.contracts.Integer], JSConTest.contracts.Boolean, {pos : [{type : 7. , effect : {type : 3. , property : "direction", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_inputsKnown75"}}}, {type : 7. , effect : {type : 3. , property : "v1", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_inputsKnown75"}}}, {type : 7. , effect : {type : 3. , property : "v2", effect : {type : 2. , name : "this", fname : "f_BinaryConstraint_prototype_inputsKnown75"}}}], neg : [], fname : "f_BinaryConstraint_prototype_inputsKnown75"}, "f_BinaryConstraint_prototype_inputsKnown75"))]);
  BinaryConstraint.prototype.removeFromGraph = JSConTest.tests.addContracts("f_BinaryConstraint_prototype_removeFromGraph77", JSConTest.tests.setVar("f_BinaryConstraint_prototype_removeFromGraph77", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "v1")) != null))) JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "v1"), "removeConstraint", [this]);;
      if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "v2")) != null))) JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "v2"), "removeConstraint", [this]);;
      JSConTest.effects.propAss(this, "direction", JSConTest.effects.propAcc(Direction, "NONE"));
  }), []), ["c_78"], "f_BinaryConstraint_prototype_removeFromGraph77"), (function  () {
    if ((this.v1 != null)) this.v1.removeConstraint(this);;
      if ((this.v2 != null)) this.v2.removeConstraint(this);;
      this.direction = Direction.NONE;
  }), true)), [], [JSConTest.tests.setVar("c_78", JSConTest.contracts.Method(bc, [], JSConTest.contracts.Undefined, {pos : [], neg : [], fname : "f_BinaryConstraint_prototype_removeFromGraph77"}, "f_BinaryConstraint_prototype_removeFromGraph77"))]);
  inherits(BinaryConstraint, ScaleConstraint);
  ScaleConstraint.prototype.addToGraph = JSConTest.tests.addContracts("f_ScaleConstraint_prototype_addToGraph80", JSConTest.tests.setVar("f_ScaleConstraint_prototype_addToGraph80", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    JSConTest.effects.mCall(JSConTest.effects.propAcc(JSConTest.effects.propAcc(JSConTest.effects.propAcc(ScaleConstraint, "superConstructor"), "prototype"), "addToGraph"), "call", [this]);
      JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "scale"), "addConstraint", [this]);
      JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "offset"), "addConstraint", [this]);
  }), []), ["c_81"], "f_ScaleConstraint_prototype_addToGraph80"), (function  () {
    ScaleConstraint.superConstructor.prototype.addToGraph.call(this);
      this.scale.addConstraint(this);
      this.offset.addConstraint(this);
  }), true)), [], [JSConTest.tests.setVar("c_81", JSConTest.contracts.Method(scc, [], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 3. , property : "v1", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_addToGraph80"}}}}}, {type : 7. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 3. , property : "v2", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_addToGraph80"}}}}}, {type : 7. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 3. , property : "scale", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_addToGraph80"}}}}}, {type : 7. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 3. , property : "offset", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_addToGraph80"}}}}}, {type : 3. , property : "direction", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_addToGraph80"}}], neg : [], fname : "f_ScaleConstraint_prototype_addToGraph80"}, "f_ScaleConstraint_prototype_addToGraph80"))]);
  ScaleConstraint.prototype.removeFromGraph = JSConTest.tests.addContracts("f_ScaleConstraint_prototype_removeFromGraph82", JSConTest.tests.setVar("f_ScaleConstraint_prototype_removeFromGraph82", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    JSConTest.effects.mCall(JSConTest.effects.propAcc(JSConTest.effects.propAcc(JSConTest.effects.propAcc(ScaleConstraint, "superConstructor"), "prototype"), "removeFromGraph"), "call", [this]);
      if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "scale")) != null))) JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "scale"), "removeConstraint", [this]);;
      if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "offset")) != null))) JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "offset"), "removeConstraint", [this]);;
  }), []), ["c_83"], "f_ScaleConstraint_prototype_removeFromGraph82"), (function  () {
    ScaleConstraint.superConstructor.prototype.removeFromGraph.call(this);
      if ((this.scale != null)) this.scale.removeConstraint(this);;
      if ((this.offset != null)) this.offset.removeConstraint(this);;
  }), true)), [], [JSConTest.tests.setVar("c_83", JSConTest.contracts.Method(scc, [], JSConTest.contracts.Undefined, {pos : [], neg : [], fname : "f_ScaleConstraint_prototype_removeFromGraph82"}, "f_ScaleConstraint_prototype_removeFromGraph82"))]);
  ScaleConstraint.prototype.markInputs = JSConTest.tests.addContracts("f_ScaleConstraint_prototype_markInputs84", JSConTest.tests.setVar("f_ScaleConstraint_prototype_markInputs84", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (mark) {
    JSConTest.effects.mCall(JSConTest.effects.propAcc(JSConTest.effects.propAcc(JSConTest.effects.propAcc(ScaleConstraint, "superConstructor"), "prototype"), "markInputs"), "call", [this, mark]);
      JSConTest.effects.propAss(JSConTest.effects.propAcc(this, "scale"), "mark", JSConTest.effects.propAss(JSConTest.effects.propAcc(this, "offset"), "mark", mark));
  }), ["mark"]), ["c_85"], "f_ScaleConstraint_prototype_markInputs84"), (function  (mark) {
    ScaleConstraint.superConstructor.prototype.markInputs.call(this, mark);
      this.scale.mark = this.offset.mark = mark;
  }), true)), [], [JSConTest.tests.setVar("c_85", JSConTest.contracts.Method(scc, [], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "direction", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_markInputs84"}}}, {type : 7. , effect : {type : 3. , property : "v1", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_markInputs84"}}}, {type : 7. , effect : {type : 3. , property : "v2", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_markInputs84"}}}, {type : 3. , property : "mark", effect : {type : 3. , property : "offset", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_markInputs84"}}}, {type : 3. , property : "mark", effect : {type : 3. , property : "scale", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_markInputs84"}}}], neg : [], fname : "f_ScaleConstraint_prototype_markInputs84"}, "f_ScaleConstraint_prototype_markInputs84"))]);
  ScaleConstraint.prototype.execute = JSConTest.tests.addContracts("f_ScaleConstraint_prototype_execute86", JSConTest.tests.setVar("f_ScaleConstraint_prototype_execute86", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "direction")) == JSConTest.effects.unbox(JSConTest.effects.propAcc(Direction, "FORWARD"))))) {
          JSConTest.effects.propAss(JSConTest.effects.propAcc(this, "v2"), "value", (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "v1"), "value")) * JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "scale"), "value")))) + JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "offset"), "value"))));
        }
        else {
          JSConTest.effects.propAss(JSConTest.effects.propAcc(this, "v1"), "value", (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "v2"), "value")) - JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "offset"), "value")))) / JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "scale"), "value"))));
        };
  }), []), ["c_87"], "f_ScaleConstraint_prototype_execute86"), (function  () {
    if ((this.direction == Direction.FORWARD)) {
          this.v2.value = ((this.v1.value * this.scale.value) + this.offset.value);
        }
        else {
          this.v1.value = ((this.v2.value - this.offset.value) / this.scale.value);
        };
  }), true)), [], [JSConTest.tests.setVar("c_87", JSConTest.contracts.Method(scc, [], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "direction", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_execute86"}}}, {type : 7. , effect : {type : 3. , property : "value", effect : {type : 3. , property : "scale", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_execute86"}}}}, {type : 7. , effect : {type : 3. , property : "value", effect : {type : 3. , property : "offset", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_execute86"}}}}, {type : 3. , property : "value", effect : {type : 3. , property : "v2", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_execute86"}}}, {type : 3. , property : "value", effect : {type : 3. , property : "v1", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_execute86"}}}], neg : [], fname : "f_ScaleConstraint_prototype_execute86"}, "f_ScaleConstraint_prototype_execute86"))]);
  ScaleConstraint.prototype.recalculate = JSConTest.tests.addContracts("f_ScaleConstraint_prototype_recalculate88", JSConTest.tests.setVar("f_ScaleConstraint_prototype_recalculate88", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    var ihn = JSConTest.effects.mCall(this, "input", []),
          out = JSConTest.effects.mCall(this, "output", []);
      JSConTest.effects.propAss(out, "walkStrength", JSConTest.effects.mCall(Strength, "weakestOf", [JSConTest.effects.propAcc(this, "strength"), JSConTest.effects.propAcc(ihn, "walkStrength")]));
      JSConTest.effects.propAss(out, "stay", ((JSConTest.effects.propAcc(ihn, "stay") && JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "scale"), "stay")) && JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "offset"), "stay")));
      if (JSConTest.effects.unbox(JSConTest.effects.propAcc(out, "stay"))) JSConTest.effects.mCall(this, "execute", []);;
  }), []), ["c_89"], "f_ScaleConstraint_prototype_recalculate88"), (function  () {
    var ihn = this.input(),
          out = this.output();
      out.walkStrength = Strength.weakestOf(this.strength, ihn.walkStrength);
      out.stay = ((ihn.stay && this.scale.stay) && this.offset.stay);
      if (out.stay) this.execute();;
  }), true)), [], [JSConTest.tests.setVar("c_89", JSConTest.contracts.Method(scc, [], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "direction", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_recalculate88"}}}, {type : 7. , effect : {type : 3. , property : "strength", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_recalculate88"}}}, {type : 7. , effect : {type : 3. , property : "stay", effect : {type : 3. , property : "scale", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_recalculate88"}}}}, {type : 7. , effect : {type : 3. , property : "value", effect : {type : 3. , property : "scale", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_recalculate88"}}}}, {type : 7. , effect : {type : 3. , property : "stay", effect : {type : 3. , property : "offset", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_recalculate88"}}}}, {type : 7. , effect : {type : 3. , property : "value", effect : {type : 3. , property : "offset", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_recalculate88"}}}}, {type : 3. , property : "value", effect : {type : 3. , property : "v2", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_recalculate88"}}}, {type : 3. , property : "value", effect : {type : 3. , property : "v1", effect : {type : 2. , name : "this", fname : "f_ScaleConstraint_prototype_recalculate88"}}}], neg : [], fname : "f_ScaleConstraint_prototype_recalculate88"}, "f_ScaleConstraint_prototype_recalculate88"))]);
  inherits(BinaryConstraint, EqualityConstraint);
  EqualityConstraint.prototype.execute = JSConTest.tests.addContracts("f_EqualityConstraint_prototype_execute91", JSConTest.tests.setVar("f_EqualityConstraint_prototype_execute91", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    JSConTest.effects.propAss(JSConTest.effects.mCall(this, "output", []), "value", JSConTest.effects.propAcc(JSConTest.effects.mCall(this, "input", []), "value"));
  }), []), ["c_92"], "f_EqualityConstraint_prototype_execute91"), (function  () {
    this.output().value = this.input().value;
  }), true)), [], [JSConTest.tests.setVar("c_92", JSConTest.contracts.Method(eqc, [], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "direction", effect : {type : 2. , name : "this", fname : "f_EqualityConstraint_prototype_execute91"}}}, {type : 7. , effect : {type : 3. , property : "v2", effect : {type : 2. , name : "this", fname : "f_EqualityConstraint_prototype_execute91"}}}, {type : 7. , effect : {type : 3. , property : "v1", effect : {type : 2. , name : "this", fname : "f_EqualityConstraint_prototype_execute91"}}}], neg : [], fname : "f_EqualityConstraint_prototype_execute91"}, "f_EqualityConstraint_prototype_execute91"))]);
  Variable.prototype.addConstraint = JSConTest.tests.addContracts("f_Variable_prototype_addConstraint94", JSConTest.tests.setVar("f_Variable_prototype_addConstraint94", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (c) {
    JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "constraints"), "add", [c]);
  }), ["c"]), ["c_95"], "f_Variable_prototype_addConstraint94"), (function  (c) {
    this.constraints.add(c);
  }), true)), [], [JSConTest.tests.setVar("c_95", JSConTest.contracts.Method(v, [c], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 2. , name : "this", fname : "f_Variable_prototype_addConstraint94"}}}}], neg : [], fname : "f_Variable_prototype_addConstraint94"}, "f_Variable_prototype_addConstraint94"))]);
  Variable.prototype.removeConstraint = JSConTest.tests.addContracts("f_Variable_prototype_removeConstraint96", JSConTest.tests.setVar("f_Variable_prototype_removeConstraint96", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (c) {
    JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "constraints"), "remove", [c]);
      if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "determinedBy")) == JSConTest.effects.unbox(c)))) JSConTest.effects.propAss(this, "determinedBy", null);;
  }), ["c"]), ["c_97"], "f_Variable_prototype_removeConstraint96"), (function  (c) {
    this.constraints.remove(c);
      if ((this.determinedBy == c)) this.determinedBy = null;;
  }), true)), [], [JSConTest.tests.setVar("c_97", JSConTest.contracts.Method(v, [c], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "length", effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 2. , name : "this", fname : "f_Variable_prototype_removeConstraint96"}}}}}, {type : 4. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 2. , name : "this", fname : "f_Variable_prototype_removeConstraint96"}}}}, {type : 3. , property : "determinedBy", effect : {type : 2. , name : "this", fname : "f_Variable_prototype_removeConstraint96"}}], neg : [], fname : "f_Variable_prototype_removeConstraint96"}, "f_Variable_prototype_removeConstraint96"))]);
  Planner.prototype.incrementalAdd = JSConTest.tests.addContracts("f_Planner_prototype_incrementalAdd99", JSConTest.tests.setVar("f_Planner_prototype_incrementalAdd99", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (c) {
    var mark = JSConTest.effects.mCall(this, "newMark", []);
      var overridden = JSConTest.effects.mCall(c, "satisfy", [mark]);
      while (JSConTest.effects.unbox((JSConTest.effects.unbox(overridden) != null))) overridden = JSConTest.effects.mCall(overridden, "satisfy", [mark]);
  }), ["c"]), ["c_100"], "f_Planner_prototype_incrementalAdd99"), (function  (c) {
    var mark = this.newMark();
      var overridden = c.satisfy(mark);
      while ((overridden != null)) overridden = overridden.satisfy(mark);
  }), true)), [], [JSConTest.tests.setVar("c_100", JSConTest.contracts.Method(pl, [c], JSConTest.contracts.Undefined, {pos : [{type : 3. , property : "currentMark", effect : {type : 2. , name : "this", fname : "f_Planner_prototype_incrementalAdd99"}}], neg : [], fname : "f_Planner_prototype_incrementalAdd99"}, "f_Planner_prototype_incrementalAdd99"))]);
  Planner.prototype.incrementalRemove = JSConTest.tests.addContracts("f_Planner_prototype_incrementalRemove101", JSConTest.tests.setVar("f_Planner_prototype_incrementalRemove101", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (c) {
    var out = JSConTest.effects.mCall(c, "output", []);
      JSConTest.effects.mCall(c, "markUnsatisfied", []);
      JSConTest.effects.mCall(c, "removeFromGraph", []);
      var unsatisfied = JSConTest.effects.mCall(this, "removePropagateFrom", [out]);
      var strength = JSConTest.effects.propAcc(Strength, "REQUIRED");
      do {
          for (var i = 0. ; JSConTest.effects.unbox((JSConTest.effects.unbox(i) < JSConTest.effects.unbox(JSConTest.effects.mCall(unsatisfied, "size", [])))); i++ )  {
            var u = JSConTest.effects.mCall(unsatisfied, "at", [i]);
            if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(u, "strength")) == JSConTest.effects.unbox(strength)))) JSConTest.effects.mCall(this, "incrementalAdd", [u]);;
          };
          strength = JSConTest.effects.mCall(strength, "nextWeaker", []);
        }
        while ((JSConTest.effects.unbox(strength) != JSConTest.effects.unbox(JSConTest.effects.propAcc(Strength, "WEAKEST"))));
  }), ["c"]), ["c_102"], "f_Planner_prototype_incrementalRemove101"), (function  (c) {
    var out = c.output();
      c.markUnsatisfied();
      c.removeFromGraph();
      var unsatisfied = this.removePropagateFrom(out);
      var strength = Strength.REQUIRED;
      do {
          for (var i = 0. ; (i < unsatisfied.size()); i++ )  {
            var u = unsatisfied.at(i);
            if ((u.strength == strength)) this.incrementalAdd(u);;
          };
          strength = strength.nextWeaker();
        }
        while ((strength != Strength.WEAKEST));
  }), true)), [], [JSConTest.tests.setVar("c_102", JSConTest.contracts.Method(pl, [c], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "length", effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 3. , property : "myOutput", effect : {type : 2. , name : "c", fname : "f_Planner_prototype_incrementalRemove101"}}}}}}, {type : 3. , property : "satisfied", effect : {type : 2. , name : "c", fname : "f_Planner_prototype_incrementalRemove101"}}, {type : 4. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 3. , property : "myOutput", effect : {type : 2. , name : "c", fname : "f_Planner_prototype_incrementalRemove101"}}}}}, {type : 3. , property : "determinedBy", effect : {type : 3. , property : "myOutput", effect : {type : 2. , name : "c", fname : "f_Planner_prototype_incrementalRemove101"}}}], neg : [], fname : "f_Planner_prototype_incrementalRemove101"}, "f_Planner_prototype_incrementalRemove101"))]);
  Planner.prototype.newMark = JSConTest.tests.addContracts("f_Planner_prototype_newMark103", JSConTest.tests.setVar("f_Planner_prototype_newMark103", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    return JSConTest.effects.unOp(3. , this, "currentMark");
  }), []), ["c_104"], "f_Planner_prototype_newMark103"), (function  () {
    return  ++this.currentMark;
  }), true)), [], [JSConTest.tests.setVar("c_104", JSConTest.contracts.Method(pl, [], JSConTest.contracts.Integer, {pos : [{type : 3. , property : "currentMark", effect : {type : 2. , name : "this", fname : "f_Planner_prototype_newMark103"}}], neg : [], fname : "f_Planner_prototype_newMark103"}, "f_Planner_prototype_newMark103"))]);
  Planner.prototype.makePlan = JSConTest.tests.addContracts("f_Planner_prototype_makePlan105", JSConTest.tests.setVar("f_Planner_prototype_makePlan105", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (sources) {
    var mark = JSConTest.effects.mCall(this, "newMark", []);
      var plan = JSConTest.effects.newCall(Plan, []);
      var todo = sources;
      while (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.mCall(todo, "size", [])) > 0. ))) {
          var c = JSConTest.effects.mCall(todo, "removeFirst", []);
          if (JSConTest.effects.unbox(((JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.mCall(c, "output", []), "mark")) != JSConTest.effects.unbox(mark)) && JSConTest.effects.mCall(c, "inputsKnown", [mark])))) {
            JSConTest.effects.mCall(plan, "addConstraint", [c]);
            JSConTest.effects.propAss(JSConTest.effects.mCall(c, "output", []), "mark", mark);
            JSConTest.effects.mCall(this, "addConstraintsConsumingTo", [JSConTest.effects.mCall(c, "output", []), todo]);
          };
        }
      return plan;
  }), ["sources"]), ["c_106"], "f_Planner_prototype_makePlan105"), (function  (sources) {
    var mark = this.newMark();
      var plan = new Plan();
      var todo = sources;
      while ((todo.size() > 0. )) {
          var c = todo.removeFirst();
          if (((c.output().mark != mark) && c.inputsKnown(mark))) {
            plan.addConstraint(c);
            c.output().mark = mark;
            this.addConstraintsConsumingTo(c.output(), todo);
          };
        }
      return plan;
  }), true)), [], [JSConTest.tests.setVar("c_106", JSConTest.contracts.Method(pl, [oc], p, {pos : [{type : 7. , effect : {type : 3. , property : "length", effect : {type : 3. , property : "elms", effect : {type : 2. , name : "sources", fname : "f_Planner_prototype_makePlan105"}}}}, {type : 3. , property : "currentMark", effect : {type : 2. , name : "this", fname : "f_Planner_prototype_makePlan105"}}], neg : [], fname : "f_Planner_prototype_makePlan105"}, "f_Planner_prototype_makePlan105"))]);
  Planner.prototype.extractPlanFromConstraints = JSConTest.tests.addContracts("f_Planner_prototype_extractPlanFromConstraints107", JSConTest.tests.setVar("f_Planner_prototype_extractPlanFromConstraints107", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (constraints) {
    var sources = JSConTest.effects.newCall(OrderedCollection, []);
      for (var i = 0. ; JSConTest.effects.unbox((JSConTest.effects.unbox(i) < JSConTest.effects.unbox(JSConTest.effects.mCall(constraints, "size", [])))); i++ )  {
          var c = JSConTest.effects.mCall(constraints, "at", [i]);
          if (JSConTest.effects.unbox((JSConTest.effects.mCall(c, "isInput", []) && JSConTest.effects.mCall(c, "isSatisfied", [])))) JSConTest.effects.mCall(sources, "add", [c]);;
        };
      return JSConTest.effects.mCall(this, "makePlan", [sources]);
  }), ["constraints"]), ["c_108"], "f_Planner_prototype_extractPlanFromConstraints107"), (function  (constraints) {
    var sources = new OrderedCollection();
      for (var i = 0. ; (i < constraints.size()); i++ )  {
          var c = constraints.at(i);
          if ((c.isInput() && c.isSatisfied())) sources.add(c);;
        };
      return this.makePlan(sources);
  }), true)), [], [JSConTest.tests.setVar("c_108", JSConTest.contracts.Method(pl, [oc], p, {pos : [{type : 7. , effect : {type : 3. , property : "length", effect : {type : 3. , property : "elms", effect : {type : 2. , name : "constraints", fname : "f_Planner_prototype_extractPlanFromConstraints107"}}}}], neg : [], fname : "f_Planner_prototype_extractPlanFromConstraints107"}, "f_Planner_prototype_extractPlanFromConstraints107"))]);
  Planner.prototype.addPropagate = JSConTest.tests.addContracts("f_Planner_prototype_addPropagate109", JSConTest.tests.setVar("f_Planner_prototype_addPropagate109", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (c, mark) {
    var todo = JSConTest.effects.newCall(OrderedCollection, []);
      JSConTest.effects.mCall(todo, "add", [c]);
      while (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.mCall(todo, "size", [])) > 0. ))) {
          var d = JSConTest.effects.mCall(todo, "removeFirst", []);
          if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.mCall(d, "output", []), "mark")) == JSConTest.effects.unbox(mark)))) {
            JSConTest.effects.mCall(this, "incrementalRemove", [c]);
            return false;
          };
          JSConTest.effects.mCall(d, "recalculate", []);
          JSConTest.effects.mCall(this, "addConstraintsConsumingTo", [JSConTest.effects.mCall(d, "output", []), todo]);
        }
      return true;
  }), ["c", "mark"]), ["c_110"], "f_Planner_prototype_addPropagate109"), (function  (c, mark) {
    var todo = new OrderedCollection();
      todo.add(c);
      while ((todo.size() > 0. )) {
          var d = todo.removeFirst();
          if ((d.output().mark == mark)) {
            this.incrementalRemove(c);
            return false;
          };
          d.recalculate();
          this.addConstraintsConsumingTo(d.output(), todo);
        }
      return true;
  }), true)), [], [JSConTest.tests.setVar("c_110", JSConTest.contracts.Method(pl, [c, JSConTest.contracts.Integer], JSConTest.contracts.Boolean, {pos : [], neg : [], fname : "f_Planner_prototype_addPropagate109"}, "f_Planner_prototype_addPropagate109"))]);
  Planner.prototype.removePropagateFrom = JSConTest.tests.addContracts("f_Planner_prototype_removePropagateFrom111", JSConTest.tests.setVar("f_Planner_prototype_removePropagateFrom111", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (out) {
    JSConTest.effects.propAss(out, "determinedBy", null);
      JSConTest.effects.propAss(out, "walkStrength", JSConTest.effects.propAcc(Strength, "WEAKEST"));
      JSConTest.effects.propAss(out, "stay", true);
      var unsatisfied = JSConTest.effects.newCall(OrderedCollection, []);
      var todo = JSConTest.effects.newCall(OrderedCollection, []);
      JSConTest.effects.mCall(todo, "add", [out]);
      while (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.mCall(todo, "size", [])) > 0. ))) {
          var v = JSConTest.effects.mCall(todo, "removeFirst", []);
          for (var i = 0. ; JSConTest.effects.unbox((JSConTest.effects.unbox(i) < JSConTest.effects.unbox(JSConTest.effects.mCall(JSConTest.effects.propAcc(v, "constraints"), "size", [])))); i++ )  {
            var c = JSConTest.effects.mCall(JSConTest.effects.propAcc(v, "constraints"), "at", [i]);
            if (JSConTest.effects.unbox( !JSConTest.effects.mCall(c, "isSatisfied", []))) JSConTest.effects.mCall(unsatisfied, "add", [c]);;
          };
          var determining = JSConTest.effects.propAcc(v, "determinedBy");
          for (var i = 0. ; JSConTest.effects.unbox((JSConTest.effects.unbox(i) < JSConTest.effects.unbox(JSConTest.effects.mCall(JSConTest.effects.propAcc(v, "constraints"), "size", [])))); i++ )  {
            var next = JSConTest.effects.mCall(JSConTest.effects.propAcc(v, "constraints"), "at", [i]);
            if (JSConTest.effects.unbox(((JSConTest.effects.unbox(next) != JSConTest.effects.unbox(determining)) && JSConTest.effects.mCall(next, "isSatisfied", [])))) {
              JSConTest.effects.mCall(next, "recalculate", []);
              JSConTest.effects.mCall(todo, "add", [JSConTest.effects.mCall(next, "output", [])]);
            };
          };
        }
      return unsatisfied;
  }), ["out"]), ["c_112"], "f_Planner_prototype_removePropagateFrom111"), (function  (out) {
    out.determinedBy = null;
      out.walkStrength = Strength.WEAKEST;
      out.stay = true;
      var unsatisfied = new OrderedCollection();
      var todo = new OrderedCollection();
      todo.add(out);
      while ((todo.size() > 0. )) {
          var v = todo.removeFirst();
          for (var i = 0. ; (i < v.constraints.size()); i++ )  {
            var c = v.constraints.at(i);
            if ( !c.isSatisfied()) unsatisfied.add(c);;
          };
          var determining = v.determinedBy;
          for (var i = 0. ; (i < v.constraints.size()); i++ )  {
            var next = v.constraints.at(i);
            if (((next != determining) && next.isSatisfied())) {
              next.recalculate();
              todo.add(next.output());
            };
          };
        }
      return unsatisfied;
  }), true)), [], [JSConTest.tests.setVar("c_112", JSConTest.contracts.Method(pl, [v], oc, {pos : [{type : 3. , property : "determinedBy", effect : {type : 2. , name : "out", fname : "f_Planner_prototype_removePropagateFrom111"}}, {type : 3. , property : "walkStrength", effect : {type : 2. , name : "out", fname : "f_Planner_prototype_removePropagateFrom111"}}, {type : 3. , property : "stay", effect : {type : 2. , name : "out", fname : "f_Planner_prototype_removePropagateFrom111"}}, {type : 3. , property : "determinedBy", effect : {type : 1. , number : 1. , fname : "f_Planner_prototype_removePropagateFrom111"}}, {type : 3. , property : "walkStrength", effect : {type : 1. , number : 1. , fname : "f_Planner_prototype_removePropagateFrom111"}}, {type : 3. , property : "stay", effect : {type : 1. , number : 1. , fname : "f_Planner_prototype_removePropagateFrom111"}}], neg : [], fname : "f_Planner_prototype_removePropagateFrom111"}, "f_Planner_prototype_removePropagateFrom111"))]);
  Planner.prototype.addConstraintsConsumingTo = JSConTest.tests.addContracts("f_Planner_prototype_addConstraintsConsumingTo113", JSConTest.tests.setVar("f_Planner_prototype_addConstraintsConsumingTo113", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (v, coll) {
    var determining = JSConTest.effects.propAcc(v, "determinedBy");
      var cc = JSConTest.effects.propAcc(v, "constraints");
      for (var i = 0. ; JSConTest.effects.unbox((JSConTest.effects.unbox(i) < JSConTest.effects.unbox(JSConTest.effects.mCall(cc, "size", [])))); i++ )  {
          var c = JSConTest.effects.mCall(cc, "at", [i]);
          if (JSConTest.effects.unbox(((JSConTest.effects.unbox(c) != JSConTest.effects.unbox(determining)) && JSConTest.effects.mCall(c, "isSatisfied", [])))) JSConTest.effects.mCall(coll, "add", [c]);;
        };
  }), ["v", "coll"]), ["c_114"], "f_Planner_prototype_addConstraintsConsumingTo113"), (function  (v, coll) {
    var determining = v.determinedBy;
      var cc = v.constraints;
      for (var i = 0. ; (i < cc.size()); i++ )  {
          var c = cc.at(i);
          if (((c != determining) && c.isSatisfied())) coll.add(c);;
        };
  }), true)), [], [JSConTest.tests.setVar("c_114", JSConTest.contracts.Method(pl, [v, oc], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "determinedBy", effect : {type : 2. , name : "v", fname : "f_Planner_prototype_addConstraintsConsumingTo113"}}}, {type : 7. , effect : {type : 3. , property : "length", effect : {type : 3. , property : "elms", effect : {type : 3. , property : "constraints", effect : {type : 2. , name : "v", fname : "f_Planner_prototype_addConstraintsConsumingTo113"}}}}}], neg : [], fname : "f_Planner_prototype_addConstraintsConsumingTo113"}, "f_Planner_prototype_addConstraintsConsumingTo113"))]);
  Plan.prototype.addConstraint = JSConTest.tests.addContracts("f_Plan_prototype_addConstraint116", JSConTest.tests.setVar("f_Plan_prototype_addConstraint116", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (c) {
    JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "v"), "add", [c]);
  }), ["c"]), ["c_117"], "f_Plan_prototype_addConstraint116"), (function  (c) {
    this.v.add(c);
  }), true)), [], [JSConTest.tests.setVar("c_117", JSConTest.contracts.Method(p, [c], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "v", effect : {type : 2. , name : "this", fname : "f_Plan_prototype_addConstraint116"}}}}], neg : [], fname : "f_Plan_prototype_addConstraint116"}, "f_Plan_prototype_addConstraint116"))]);
  Plan.prototype.size = JSConTest.tests.addContracts("f_Plan_prototype_size118", JSConTest.tests.setVar("f_Plan_prototype_size118", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    return JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "v"), "size", []);
  }), []), ["c_119"], "f_Plan_prototype_size118"), (function  () {
    return this.v.size();
  }), true)), [], [JSConTest.tests.setVar("c_119", JSConTest.contracts.Method(p, [], JSConTest.contracts.Integer, {pos : [{type : 7. , effect : {type : 3. , property : "length", effect : {type : 3. , property : "elms", effect : {type : 3. , property : "v", effect : {type : 2. , name : "this", fname : "f_Plan_prototype_size118"}}}}}], neg : [], fname : "f_Plan_prototype_size118"}, "f_Plan_prototype_size118"))]);
  Plan.prototype.constraintAt = JSConTest.tests.addContracts("f_Plan_prototype_constraintAt120", JSConTest.tests.setVar("f_Plan_prototype_constraintAt120", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  (index) {
    return JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "v"), "at", [index]);
  }), ["index"]), ["c_121"], "f_Plan_prototype_constraintAt120"), (function  (index) {
    return this.v.at(index);
  }), true)), [], [JSConTest.tests.setVar("c_121", JSConTest.contracts.Method(p, [JSConTest.contracts.Integer], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 4. , effect : {type : 3. , property : "elms", effect : {type : 3. , property : "v", effect : {type : 2. , name : "this", fname : "f_Plan_prototype_constraintAt120"}}}}}], neg : [], fname : "f_Plan_prototype_constraintAt120"}, "f_Plan_prototype_constraintAt120"))]);
  Plan.prototype.execute = JSConTest.tests.addContracts("f_Plan_prototype_execute122", JSConTest.tests.setVar("f_Plan_prototype_execute122", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function  () {
    for (var i = 0. ; JSConTest.effects.unbox((JSConTest.effects.unbox(i) < JSConTest.effects.unbox(JSConTest.effects.mCall(this, "size", [])))); i++ )  {
          var c = JSConTest.effects.mCall(this, "constraintAt", [i]);
          JSConTest.effects.mCall(c, "execute", []);
        };
  }), []), ["c_123"], "f_Plan_prototype_execute122"), (function  () {
    for (var i = 0. ; (i < this.size()); i++ )  {
          var c = this.constraintAt(i);
          c.execute();
        };
  }), true)), [], [JSConTest.tests.setVar("c_123", JSConTest.contracts.Method(p, [], JSConTest.contracts.Undefined, {pos : [{type : 7. , effect : {type : 3. , property : "length", effect : {type : 3. , property : "elms", effect : {type : 3. , property : "v", effect : {type : 2. , name : "this", fname : "f_Plan_prototype_execute122"}}}}}], neg : [], fname : "f_Plan_prototype_execute122"}, "f_Plan_prototype_execute122"))]);
  var planner = null;