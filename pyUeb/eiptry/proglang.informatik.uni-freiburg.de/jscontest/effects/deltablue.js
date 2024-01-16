/** 
 * Annotated version for JSConTest.
 * Contracts were created manually. The effects were infered. 
 * We also have to restructure the code a little bit.
 * - The mothod inherits attached to the object.prototype is replaced 
 *   by a function. This is due to restriction of JsTestDriver, which 
 *   was used for the mutation testing. 
 *   If the global object prototype is extended in a JsTestDriver test case,
 *   the test suite throws an error due to code of jquerrys ajax library, used
 *   by JsTestDriver. This happens in JsTestDriver 1.3.2.
 * - The benchmark is executed not using the BenchmarkSuite constructor. Therefor
 *   it is removed.
 */


// Copyright 2008 the V8 project authors. All rights reserved.
// Copyright 1996 John Maloney and Mario Wolczko.

// This program is free software; you can redistribute it and/or modify
// it under the terms of the GNU General Public License as published by
// the Free Software Foundation; either version 2 of the License, or
// (at your option) any later version.
//
// This program is distributed in the hope that it will be useful,
// but WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
// GNU General Public License for more details.
//
// You should have received a copy of the GNU General Public License
// along with this program; if not, write to the Free Software
// Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


// This implementation of the DeltaBlue benchmark is derived
// from the Smalltalk implementation by John Maloney and Mario
// Wolczko. Some parts have been translated directly, whereas
// others have been modified more aggresively to make it feel
// more like a JavaScript program.


/**
 * A JavaScript implementation of the DeltaBlue constraint-solving
 * algorithm, as described in:
 *
 * "The DeltaBlue Algorithm: An Incremental Constraint Hierarchy Solver"
 *   Bjorn N. Freeman-Benson and John Maloney
 *   January 1990 Communications of the ACM,
 *   also available as University of Washington TR 89-08-06.
 *
 * Beware: this benchmark is written in a grotesque style where
 * the constraint model is built by side-effects from constructors.
 * I've kept it this way to avoid deviating too much from the original
 * implementation.
 */


function inherits(shuper, c) {
    function Inheriter() { ;
			 }
    Inheriter.prototype = shuper.prototype;
    c.prototype = new Inheriter();
    c.superConstructor = shuper;
};

function OrderedCollection() {
	this.elms = new Array();
}

OrderedCollection.prototype.add = function(elm) 
    /*c js:oc.({}) -> undefined with [this.elms.@]  ~noTests */ {
    this.elms.push(elm);
};

OrderedCollection.prototype.at = function(index) 
/*c js:oc.(int) -> top with [this.elms.?.@] ~noTests */ 
{     
	return this.elms[index];
};

OrderedCollection.prototype.size = function() 
/*c js:oc.() -> int with [this.elms."length".@] ~noTests */ 
{
	return this.elms.length;
};

OrderedCollection.prototype.removeFirst = function()
    /*c js:oc.() -> {} with [this.elms.@] ~noTests */ {
	return this.elms.pop();
};

OrderedCollection.prototype.remove = function(elm) 
     /*c js:oc.({}) -> undefined with [this.elms."length".@,this.elms.?] ~noTests */ { 
	var index = 0, skipped = 0;
	for ( var i = 0; i < this.elms.length; i++) {
		var value = this.elms[i];
		if (value != elm) {
			this.elms[index] = value;
			index++;
		} else {
			skipped++;
		}
	}
	for ( var i = 0; i < skipped; i++)
		this.elms.pop();
};

function Strength(strengthValue, name) {
	this.strengthValue = strengthValue;
	this.name = name;
}

Strength.stronger = function(s1, s2) 
/*c js:StrengthClass.(js:s,js:s) -> bool with [s1.strengthValue, s2.strengthValue]  ~noTests */ {  
	return s1.strengthValue < s2.strengthValue;
};

Strength.weaker = function(s1, s2) 
/*c js:StrengthClass.(js:s,js:s) -> bool ~noAsserts  ~noTests */ {
	return s1.strengthValue > s2.strengthValue;
};

Strength.weakestOf = function(s1, s2) 
/*c js:StrengthClass.(js:s,js:s) -> js:s ~noAsserts  ~noTests */ {
	return this.weaker(s1, s2) ? s1 : s2;
};

Strength.strongest = function(s1, s2) 
/*c js:StrengthClass.(js:s,js:s) -> js:s ~noAsserts  ~noTests */ {
	return this.stronger(s1, s2) ? s1 : s2;
};

Strength.prototype.nextWeaker = function() {
	switch (this.strengthValue) {
	case 0:
		return Strength.WEAKEST;
	case 1:
		return Strength.WEAK_DEFAULT;
	case 2:
		return Strength.NORMAL;
	case 3:
		return Strength.STRONG_DEFAULT;
	case 4:
		return Strength.PREFERRED;
	case 5:
		return Strength.REQUIRED;
	}
};

Strength.REQUIRED = new Strength(0, "required");
Strength.STONG_PREFERRED = new Strength(1, "strongPreferred");
Strength.PREFERRED = new Strength(2, "preferred");
Strength.STRONG_DEFAULT = new Strength(3, "strongDefault");
Strength.NORMAL = new Strength(4, "normal");
Strength.WEAK_DEFAULT = new Strength(5, "weakDefault");
Strength.WEAKEST = new Strength(6, "weakest");

function Constraint(strength) {
	this.strength = strength;
}

Constraint.prototype.addConstraint = function() 
/*c js:c.() -> undefined  with [this.v1.constraints.elms.@,
this.v2.constraints.elms.@,this.myOutput.constraints.elms.@	,
this.scale.constraints.elms.@	,this.offset.constraints.elms.@,
this.direction	,this.satisfied]  ~noTests */ 
{ 
	this.addToGraph();
	planner.incrementalAdd(this);
};

Constraint.prototype.satisfy = function(mark) 
/*c js:c.(int) -> js:nullc  
with [this.v1.mark.@,this.v1.walkStrength.@,this.v2.mark.@,this.v2.walkStrength.@,
this.strength.@,this.myOutput.mark.@,this.myOutput.walkStrength.@,
this.direction,this.satisfied,this.offset.mark,this.scale.mark] ~noTests */ 
{
	this.chooseMethod(mark);
	if (!this.isSatisfied()) {
		if (this.strength == Strength.REQUIRED)
			throw("Could not satisfy a required constraint!");
		return null;
	}
	this.markInputs(mark);
	var out = this.output();
	var overridden = out.determinedBy;
	if (overridden != null)
		overridden.markUnsatisfied();
	out.determinedBy = this;
	if (!planner.addPropagate(this, mark))
		throw("Cycle encountered");
	out.mark = mark;
	return overridden;
};

Constraint.prototype.destroyConstraint = function() 
/*c js:c.() -> undefined with [this.satisfied.@]  ~noTests */ 
{
	if (this.isSatisfied())
		planner.incrementalRemove(this);
	else
		this.removeFromGraph();
};

Constraint.prototype.isInput = function() 
     /*c js:c.() -> bool   ~noTests */ {
	return false;
};

function UnaryConstraint(v, strength) {
	UnaryConstraint.superConstructor.call(this, strength);
	this.myOutput = v;
	this.satisfied = false;
	this.addConstraint();
}

inherits(Constraint, UnaryConstraint);

UnaryConstraint.prototype.addToGraph = function() 
     /*c js:uc.() -> undefined 
      with [this.myOutput.constraints.elms.@,this.satisfied]  ~noTests */ {
	this.myOutput.addConstraint(this);
	this.satisfied = false;
};

UnaryConstraint.prototype.chooseMethod = function(mark) 
     /*c js:uc.(int) -> undefined 
     with [this.myOutput.mark.@,this.myOutput.walkStrength.@,this.strength.@,
	this.satisfied]  ~noTests */ {
	this.satisfied = (this.myOutput.mark != mark)
			&& Strength.stronger(this.strength, this.myOutput.walkStrength);
};

UnaryConstraint.prototype.isSatisfied = function() 
     /*c js:uc.() -> bool with [this.satisfied.@]  ~noTests */ {
	return this.satisfied;
};


UnaryConstraint.prototype.markInputs = function(mark) 
     /*c js:uc.(int) -> undefined   ~noTests */ {
     return;
};

UnaryConstraint.prototype.output = function() 
     /*c js:uc.() -> js:v with [this.myOutput.@]  ~noTests */ {     
	return this.myOutput;
};

UnaryConstraint.prototype.recalculate = function() 
     /*c js:uc.() -> undefined 
     with [this.strength.@,this.myOutput.walkStrength,this.myOutput.stay]  
     ~noTests */ {
	this.myOutput.walkStrength = this.strength;
	this.myOutput.stay = !this.isInput();
	if (this.myOutput.stay)
		this.execute(); 
};

UnaryConstraint.prototype.markUnsatisfied = function() 
     /*c js:uc.() -> undefined  with [this.satisfied] ~noTests */ {
	this.satisfied = false;
};

UnaryConstraint.prototype.inputsKnown = function() 
     /*c js:uc.() -> bool   ~noTests */ {
	return true;
};

UnaryConstraint.prototype.removeFromGraph = function() 
     /*c js:uc.() -> undefined 
         with [this.myOutput.constraints.elms."length".@,
	       this.myOutput.constraints.elms.?,
	       this.myOutput.determinedBy,
	       this.satisfied]  ~noTests */ {
	if (this.myOutput != null)
		this.myOutput.removeConstraint(this);
	this.satisfied = false;
};

function StayConstraint(v, str) {
	StayConstraint.superConstructor.call(this, v, str);
}

inherits(UnaryConstraint, StayConstraint);

StayConstraint.prototype.execute = function() 
     /*c js:stc.() -> undefined   ~noTests */ {
     return;
};


function EditConstraint(v, str) {
	EditConstraint.superConstructor.call(this, v, str);
}

inherits(UnaryConstraint, EditConstraint);

EditConstraint.prototype.isInput = function() 
     /*c js:edc.() -> bool   ~noTests */ {
	return true;
};

EditConstraint.prototype.execute = function() 
     /*c js:edc.() -> undefined   ~noTests */ {
     return;
};


var Direction = new Object();
Direction.NONE = 0;
Direction.FORWARD = 1;
Direction.BACKWARD = -1;

function BinaryConstraint(var1, var2, strength) {
	BinaryConstraint.superConstructor.call(this, strength);
	this.v1 = var1;
	this.v2 = var2;
	this.direction = Direction.NONE;
	this.addConstraint();
}

inherits(Constraint, BinaryConstraint);

BinaryConstraint.prototype.chooseMethod = function(mark)
     /*c js:bc.(int) -> undefined 
     with [this.v1.mark.@	,	this.v1.walkStrength.@	,	this.v2.mark.@	,	this.v2.walkStrength.@	,	this.strength.@	,	this.direction]     
       ~noTests */ {
	if (this.v1.mark == mark) {
		this.direction = (this.v1.mark != mark && Strength.stronger(
				this.strength, this.v2.walkStrength)) ? Direction.FORWARD
				: Direction.NONE;
	}
	if (this.v2.mark == mark) {
		this.direction = (this.v1.mark != mark && Strength.stronger(
				this.strength, this.v1.walkStrength)) ? Direction.BACKWARD
				: Direction.NONE;
	}
	if (Strength.weaker(this.v1.walkStrength, this.v2.walkStrength)) {
		this.direction = Strength.stronger(this.strength, this.v1.walkStrength) ? Direction.BACKWARD
				: Direction.NONE;
	} else {
		this.direction = Strength.stronger(this.strength, this.v2.walkStrength) ? Direction.FORWARD
				: Direction.BACKWARD;
	}
};

BinaryConstraint.prototype.addToGraph = function() 
     /*c js:bc.() -> undefined with [this.v1.constraints.elms.@,
     this.v2.constraints.elms.@,this.direction]  ~noTests */ {
	this.v1.addConstraint(this);
	this.v2.addConstraint(this);
	this.direction = Direction.NONE;
};


BinaryConstraint.prototype.isSatisfied = function() 
     /*c js:bc.() -> bool with [this.direction.@]  ~noTests */ {
	return this.direction != Direction.NONE;
};

BinaryConstraint.prototype.markInputs = function(mark) 
     /*c js:bc.(int) -> undefined with [this.direction.@	,	this.v1.@	,	this.v2.@]  ~noTests */ {
	this.input().mark = mark;
};

BinaryConstraint.prototype.input = function() 
     /*c js:bc.() -> js:v with [this.direction.@	,	this.v1.@	,	this.v2.@]  ~noTests */ {
	return (this.direction == Direction.FORWARD) ? this.v1 : this.v2;
};

BinaryConstraint.prototype.output = function() 
     /*c js:bc.() -> js:v with [this.direction.@	,	this.v2.@	,	this.v1.@]  ~noTests */ {
	return (this.direction == Direction.FORWARD) ? this.v2 : this.v1;
};

BinaryConstraint.prototype.recalculate = function() 
     /*c js:bc.() -> undefined with [this.direction.@	,	this.v1.@	,	this.v2.@	,	this.strength.@]  ~noTests */ {
	var ihn = this.input(), out = this.output();
	out.walkStrength = Strength.weakestOf(this.strength, ihn.walkStrength);
	out.stay = ihn.stay;
	if (out.stay)
		this.execute();
};

BinaryConstraint.prototype.markUnsatisfied = function() 
     /*c js:bc.() -> undefined  with [this.direction] ~noTests */ {
	this.direction = Direction.NONE;
};

BinaryConstraint.prototype.inputsKnown = function(mark) 
     /*c js:bc.(int) -> bool with [this.direction.@	,	this.v1.@	,	this.v2.@]  ~noTests */ {
	var i = this.input();
	return i.mark == mark || i.stay || i.determinedBy == null;
};

BinaryConstraint.prototype.removeFromGraph = function() 
     /*c js:bc.() -> undefined   ~noTests */ {
	if (this.v1 != null)
		this.v1.removeConstraint(this);
	if (this.v2 != null)
		this.v2.removeConstraint(this);
	this.direction = Direction.NONE;
};

function ScaleConstraint(src, scale, offset, dest, strength) {
	this.direction = Direction.NONE;
	this.scale = scale;
	this.offset = offset;
	ScaleConstraint.superConstructor.call(this, src, dest, strength);
}

inherits(BinaryConstraint, ScaleConstraint);

ScaleConstraint.prototype.addToGraph = function() 
     /*c js:scc.() -> undefined with [this.v1.constraints.elms.@	,	this.v2.constraints.elms.@	,	this.scale.constraints.elms.@	,	this.offset.constraints.elms.@	,	this.direction]  ~noTests */ {
	ScaleConstraint.superConstructor.prototype.addToGraph.call(this);
	this.scale.addConstraint(this);
	this.offset.addConstraint(this);
};

ScaleConstraint.prototype.removeFromGraph = function() 
     /*c js:scc.() -> undefined  ~noTests */ {
	ScaleConstraint.superConstructor.prototype.removeFromGraph.call(this);
	if (this.scale != null)
		this.scale.removeConstraint(this);
	if (this.offset != null)
		this.offset.removeConstraint(this);
};

ScaleConstraint.prototype.markInputs = function(mark) 
     /*c js:scc.() -> undefined  with [this.direction.@	,	this.v1.@	,	this.v2.@	,	this.offset.mark	,	this.scale.mark] ~noTests */ {
    ScaleConstraint.superConstructor.prototype.markInputs.call(this, mark);
    this.scale.mark = this.offset.mark = mark;
};

ScaleConstraint.prototype.execute = function() 
     /*c js:scc.() -> undefined 
     with [this.direction.@	,	this.scale.value.@	,	this.offset.value.@	,	this.v2.value	,	this.v1.value]  ~noTests */ {
	if (this.direction == Direction.FORWARD) {
		this.v2.value = this.v1.value * this.scale.value + this.offset.value;
	} else {
		this.v1.value = (this.v2.value - this.offset.value) / this.scale.value;
	}
};

ScaleConstraint.prototype.recalculate = function() 
     /*c js:scc.() -> undefined with [this.direction.@	,	this.strength.@	,	this.scale.stay.@	,	this.scale.value.@	,	this.offset.stay.@	,	this.offset.value.@	,	this.v2.value	,	this.v1.value
]  ~noTests */ {
	var ihn = this.input(), out = this.output();
	out.walkStrength = Strength.weakestOf(this.strength, ihn.walkStrength);
	out.stay = ihn.stay && this.scale.stay && this.offset.stay;
	if (out.stay)
		this.execute();
};

function EqualityConstraint(var1, var2, strength) {
	EqualityConstraint.superConstructor.call(this, var1, var2, strength);
}

inherits(BinaryConstraint, EqualityConstraint);

EqualityConstraint.prototype.execute = function() 
     /*c js:eqc.() -> undefined with [this.direction.@	,	this.v2.@	,	this.v1.@] ~noTests */ {
	this.output().value = this.input().value;
};

function Variable(name, initialValue) {
	this.value = initialValue || 0;
	this.constraints = new OrderedCollection();
	this.determinedBy = null;
	this.mark = 0;
	this.walkStrength = Strength.WEAKEST;
	this.stay = true;
	this.name = name;
}

Variable.prototype.addConstraint = function(c) 
/*c js:v.(js:c) -> undefined with [this.constraints.elms.@] ~noTests */ {
	this.constraints.add(c);
};

Variable.prototype.removeConstraint = function(c) 
/*c js:v.(js:c) -> undefined 
    with [this.constraints.elms."length".@,
          this.constraints.elms.?,
	  this.determinedBy] ~noTests */ {
	this.constraints.remove(c);
	if (this.determinedBy == c)
		this.determinedBy = null;
};

function Planner() {
	this.currentMark = 0;
}

Planner.prototype.incrementalAdd = function(c) 
     /*c js:pl.(js:c) -> undefined with [this.currentMark] ~noTests */ {
	var mark = this.newMark();
	var overridden = c.satisfy(mark);
	while (overridden != null)
		overridden = overridden.satisfy(mark);
};

Planner.prototype.incrementalRemove = function(c) 
     /*c js:pl.(js:c) -> undefined 
         with [c.myOutput.constraints.elms."length".@,
	       c.satisfied,
	       c.myOutput.constraints.elms.?,
	       c.myOutput.determinedBy] ~noTests */ {
	var out = c.output();
	c.markUnsatisfied();
	c.removeFromGraph();
	var unsatisfied = this.removePropagateFrom(out);
	var strength = Strength.REQUIRED;
	do {
		for ( var i = 0; i < unsatisfied.size(); i++) {
			var u = unsatisfied.at(i);
			if (u.strength == strength)
				this.incrementalAdd(u);
		}
		strength = strength.nextWeaker();
	} while (strength != Strength.WEAKEST);
};

Planner.prototype.newMark = function() 
     /*c js:pl.() -> int with [this.currentMark] ~noTests */ {
	return ++this.currentMark;
};

Planner.prototype.makePlan = function(sources) 
     /*c js:pl.(js:oc) -> js:p with[sources.elms."length".@,this.currentMark] ~noTests */ {
	var mark = this.newMark();
	var plan = new Plan();
	var todo = sources;
	while (todo.size() > 0) {
		var c = todo.removeFirst();
		if (c.output().mark != mark && c.inputsKnown(mark)) {
			plan.addConstraint(c);
			c.output().mark = mark;
			this.addConstraintsConsumingTo(c.output(), todo);
		}
	}
	return plan;
};

Planner.prototype.extractPlanFromConstraints = function(constraints) 
      /*c js:pl.(js:oc) -> js:p with [constraints.elms."length".@] ~noTests */ {

	var sources = new OrderedCollection();
	for ( var i = 0; i < constraints.size(); i++) {
		var c = constraints.at(i);
		if (c.isInput() && c.isSatisfied())
			sources.add(c);
	}
	return this.makePlan(sources);
};

Planner.prototype.addPropagate = function(c, mark) 
     /*c js:pl.(js:c,integer) -> bool ~noTests */ {

	var todo = new OrderedCollection();
	todo.add(c);
	while (todo.size() > 0) {
		var d = todo.removeFirst();
		if (d.output().mark == mark) {
			this.incrementalRemove(c);
			return false;
		}
		d.recalculate();
		this.addConstraintsConsumingTo(d.output(), todo);
	}
	return true;
};

Planner.prototype.removePropagateFrom = function(out) 
   /*c js:pl.(js:v) -> js:oc with [out.determinedBy	,	out.walkStrength	,	out.stay, $1.determinedBy	,	$1.walkStrength	,	$1.stay] ~noTests */ 
{
	out.determinedBy = null;
	out.walkStrength = Strength.WEAKEST;
	out.stay = true;
	var unsatisfied = new OrderedCollection();
	var todo = new OrderedCollection();
	todo.add(out);
	while (todo.size() > 0) {
		var v = todo.removeFirst();
		for ( var i = 0; i < v.constraints.size(); i++) {
			var c = v.constraints.at(i);
			if (!c.isSatisfied())
				unsatisfied.add(c);
		}
		var determining = v.determinedBy;
		for ( var i = 0; i < v.constraints.size(); i++) {
			var next = v.constraints.at(i);
			if (next != determining && next.isSatisfied()) {
				next.recalculate();
				todo.add(next.output());
			}
		}
	}
	return unsatisfied;
};

Planner.prototype.addConstraintsConsumingTo = function(v, coll) 
     /*c js:pl.(js:v,js:oc) -> undefined  with [v.determinedBy.@,v.constraints.elms."length".@] ~noTests */ {
	var determining = v.determinedBy;
	var cc = v.constraints;
	for ( var i = 0; i < cc.size(); i++) {
		var c = cc.at(i);
		if (c != determining && c.isSatisfied())
			coll.add(c);
	}
};


function Plan() {
	this.v = new OrderedCollection();
}

Plan.prototype.addConstraint = function(c) 
     /*c js:p.(js:c) -> undefined with [this.v.elms.@] ~noTests */ { 
	this.v.add(c);
};

Plan.prototype.size = function() 
     /*c js:p.() -> int with [this.v.elms."length".@] ~noTests */ {
	return this.v.size();
};

Plan.prototype.constraintAt = function(index) 
     /*c js:p.(integer) -> undefined with [this.v.elms.?.@] ~noTests */ { 
	return this.v.at(index);
};

Plan.prototype.execute = function() 
     /*c js:p.() -> undefined with [this.v.elms."length".@] ~noTests */ { 
	for ( var i = 0; i < this.size(); i++) {
		var c = this.constraintAt(i);
		c.execute();
	}
};

/*c int -> undefined  ~noTests */ 
function chainTest(n) {
	planner = new Planner();
	var prev = null, first = null, last = null;

	for ( var i = 0; i <= n; i++) {
		var name = "v" + i;
		var v = new Variable(name);
		if (prev != null)
			new EqualityConstraint(prev, v, Strength.REQUIRED);
		if (i == 0)
			first = v;
		if (i == n)
			last = v;
		prev = v;
	}

	new StayConstraint(last, Strength.STRONG_DEFAULT);
	var edit = new EditConstraint(first, Strength.PREFERRED);
	var edits = new OrderedCollection();
	edits.add(edit);
	var plan = planner.extractPlanFromConstraints(edits);
	for ( var i = 0; i < 100; i++) {
		first.value = i;
		plan.execute();
		if (last.value != i)
			throw("Chain test failed.");
	}
}

/*c int -> undefined ~noTests */ 
function projectionTest(n) {
     planner = new Planner();
	var scale = new Variable("scale", 10);
	var offset = new Variable("offset", 1000);
	var src = null, dst = null;

	var dests = new OrderedCollection();
	for ( var i = 0; i < n; i++) {
		src = new Variable("src" + i, i);
		dst = new Variable("dst" + i, i);
		dests.add(dst);
		new StayConstraint(src, Strength.NORMAL);
		new ScaleConstraint(src, scale, offset, dst, Strength.REQUIRED);
	}

	change(src, 17);
	if (dst.value != 1170)
		throw("Projection 1 failed");
	change(dst, 1050);
	if (src.value != 5)
		throw("Projection 2 failed");
	change(scale, 5);
	for ( var i = 0; i < n - 1; i++) {
		if (dests.at(i).value != i * 5 + 1000)
			throw("Projection 3 failed");
	}
	change(offset, 2000);
	for ( var i = 0; i < n - 1; i++) {
		if (dests.at(i).value != i * 5 + 2000)
			throw("Projection 4 failed");
	}
}

 /*c (js:v,int)-> undefined with [v.value] ~noTests */
function change(v, newValue) {
	var edit = new EditConstraint(v, Strength.PREFERRED);
	var edits = new OrderedCollection();
	edits.add(edit);
	var plan = planner.extractPlanFromConstraints(edits);
	for ( var i = 0; i < 10; i++) {
		v.value = newValue;
		plan.execute();
	}
	edit.destroyConstraint();
}

var planner = null;

 /*c undefined -> undefined  */
function deltaBlue() {
	chainTest(100);
	projectionTest(100);
}


