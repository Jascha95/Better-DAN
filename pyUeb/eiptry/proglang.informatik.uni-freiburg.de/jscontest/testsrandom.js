/* 
Copyright 2010 Phillip Heidegger. All rights reserved.
   
Redistribution and use in source and binary forms, with or without
modification, are permitted provided that the following conditions are
met:

   1. Redistributions of source code must retain the above copyright
      notice, this list of conditions and the following disclaimer.

   2. Redistributions in binary form must reproduce the above
      copyright notice, this list of conditions and the following
      disclaimer in the documentation and/or other materials
      provided with the distribution.

THIS SOFTWARE IS PROVIDED BY PHILLIP HEIDEGGER ``AS IS'' AND ANY
EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL PHILLIP HEIDEGGER OR
CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL,
EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO,
PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR
PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF
LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING
NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

The views and conclusions contained in the software and documentation
are those of the authors and should not be interpreted as representing
official policies, either expressed or implied, of Phillip Heidegger.
*/

/* Version: 0.1.2 */

var TESTS = 

(function (T) {
  /* private variabels */
  /* collects all the tests that are added by TESTS.add(v,c,t); */
  var tests = {};
  /* collects counterexamples */
  var counterexp = {};
  /* the actual module */
  var module = "";
  /* MAXINT constant, computed in initialize */
  var MAXINT;
  /* Array with all the log observers */
  var logger = [];
  /* Number of tests after which events in the browser are handled */
  var testCountPerStep = 5000;
  
  /* Contract Types */
  /* Each contract must have a type that is 
   * one of these.
   * They will be used in a further version to implenent 
   * rewriting of complex contracts, e.g. intersections.
   */
  /* No type is given */
  var ctNoType = 0;
  /* Basic Contracts: Singletons, String, Bool, Number, ...*/
  var ctBasic = 1; 
  /* Objects */
  var ctObject = 2;
  /* Arrays */
  var ctArray = 3;
  /* Functions */
  var ctFunction = 4;
  /* Union, Intersection, ... */
  var ctComplex = 5;
  /* Names */
  var ctName = 6;
  function makeContractType(ct) {
    if (isInt(ct) && (ct >= 0) && (ct < 7)) {
      return ct;
    } else {
      return 0;
    };
  };
  
  /********** checks **********/
  function isNull(v) { return v === null; };
  function isTrue(v) { return v === true; };
  function isFalse(v) { return v === false; };
  function isBoolean(v) { return typeof(v) === 'boolean' };
  function isUndefined(v) { return v === undefined; };
  function isNumber(v) { return (typeof(v) === 'number' && (!isNaN(v))); };
  function isInt(v) {
    return (typeof(v) === 'number' && (!isNaN(v)) && (Math.floor(v) === v));
  };
  function isIInt(low,high,v) {
    return ((low <= v) && (v <= high) && isInt(v));
  };
  function isNInt(low,high,v) {
    return ((low <= v) && (v <= high) && isNumber(v));
  };
  function isString(v) { return typeof(v) === 'string' };
  function isObject(v,pl) {
    if (v === null) return false;
    var t = typeof(v);
    if (!((t === 'object') || (t === 'function'))) return false;
    for (var j in pl) {
      var p = pl[j];
      if (p.contract && !p.contract.check(v[p.name])) return false;
    };
    return true;
  };
  function isSArray(obj) {
    if (!obj) return false;
    return (obj.constructor.toString().indexOf("Array") !== -1);    
  };
  function isArray(obj, type) {
    if (!obj) return false;
    if (type) {
      for (var j = 0; j < obj.length; ++j) {
        if (!(type.check(obj[j]))) return false;
      };
    };
    return (obj.constructor.toString().indexOf("Array") !== -1);    
  };
  function isFunction(v) {
    return typeof(v) === 'function';
  };

  /********** generator **********/
  /* You can call every generate without any parmeters.
     Then all parameters are choosen randomly. 
     If you specify the parameters, they are taken to
     account, and the generated value fulfills then
     restrictions represented by them. */
  function genNull() { return null; };
  function genUndefined() { return undefined; };
  function genBoolean() {
    if (Math.random() < 0.5) {
      return true;
    } else {
      return false;
    };
  };
  function genInt() {
    var ri = Math.round(Math.random() * MAXINT);
    if (Math.random() < 0.5) {
      ri = ri * -1;
      ri = ri - 1;
    };
    return ri;
  };
  function genIInt(low, high) {
    if (isInt(low) && isInt(high)) {
      return Math.round(Math.random() * (high - low)) + low;
    } else {
      return genInt();
    };
  };
  function genNInt(low,high) {
    if (isNumber(low) && isNumber(high)) {
      return (Math.random() * (high - low)) + low;
    } else {
      return genNumber();
    };
  };
  function genNumber(low, high) {
    if (isNumber(low) && isNumber(high)) {
      // generate random double in range
      return Math.random() * (high - low) + low;
    } else {
      if ((!isNumber(low)) && (!isNumber(high))) {
        // generate random double
        // TODO
        return 0;
      } else {
        // generate random double in range [0,p], where p = low or high
        return genNumber(0,(0 + low) || (0 + high));
      };
    };
  };
  function genLength() {
    var i = 0;
    while (true) {
      if (Math.random() < 0.8) {
        i++;
      } else {
        break;
      }
    };
    return i;
  };
  function genTree(pred,leafGen,nodeGen,p,functions) {
    function genNode(p) {
      var op = unSafePick(nodeGen);
      var tl = [];
      var arity = op.arity;
      var newp = p / arity;
      for (var j = 0; j < arity; ++j) {
        tl.push(gen(newp));
      };
      var r = op.f.apply(null,tl);
      if (pred(r)) {
        return r;
      } else {
        return gen(p * 0.9);
      };
    };
    function gen(p) {
      if (Math.random() < p) {
        var r = genNode(p);
        return r;
      };
      if (functions) {
        var r = unSafePick(leafGen)();
      } else {
        var r = unSafePick(leafGen);
        return r;
      }
      if (pred(r)) {
        return r;
      };
      return gen(p * 0.9);
    };
    if (!isSArray(leafGen)) {
      throw "Call genTree with an array that can generate leafs";
    };
    if (!isSArray(nodeGen) || nodeGen.length < 1) {
      return pick(leafGen)();
    };
    if (p === undefined) p = 0.5;
    if (!isNumber(p)) p = 0.5;
    var q = 0.5;
    if (leafGen.length > 0) {
      q = 1 / leafGen.length;
    };
    if (!functions) {
      for (var i = 0; i < leafGen.length; i++) {
        if (!pred(leafGen[i])) throw "Leaf does not fulfill predicate";
      };
    };
    return gen(p);
  };
  function genAInt(iList,fList,p) {
    if (!isSArray(iList)) return genInt();
    return genTree(isInt,iList,fList,p);
  }
  function genAIntOld(iList,fList,p) {
    function genFInt(p) {
      var op = pick(fList);
      var tl = [];
      var arity = op.arity;
      var newp = p / arity;
      for (j = 0; j < arity; ++j) {
        tl.push(gen(newp));
      };
      var r = op.f.apply(null,tl);
      if (isInt(r)) {
        return r;
      } else {
        return gen(p * 0.9);
      };
    };
    function gen(p) {
      if (Math.random() < p) {
        var r = genFInt(p);
        return r;
      };
      if (Math.random() < q) {
        return genInt();
      };
      var r = pick(iList);      
      if (isInt(r)) {
        return r;
      };
      return gen(p * 0.9);
    };

    if (!fList || !fList.length || fList.length < 1) {
      return pick(iList);
    };
    if (p === undefined) p = 0.5;
    if (!isNumber(p)) p = 0.5;
    var q = 0.5;
    if (iList.length > 0) {
      q = 1 / iList.length;
    };
    return gen(p);
  };
  function genEPL(gS) {
    var l = genLength();
    pl = [];
    if (!gS) gS = genString;
    for (var j = 0; j < l; ++j) {
      var pn = gS();
      pl.push({name: pn, contract: T.TopOUndef});
    };
    return pl;
  };
  function genSEPL() {
    return genEPL(genPropString);
  };
  function genRPL() {
    // TODO
    return [];
  };
  function genPropString() {
    return genString('abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0987654321');
  };
  function genString(chars,length) {
    if (!chars) {
      chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0987654321!@#$%^&*()_+-=[]\\{}|'\",/<>?~`";
    };
    var i = 0;
    if (!length) i = genLength()
    else i = length;
    if (i < 1) {
      return "";
    } else {
      var r = "";
      for (var j = 0; j< i; ++j) {
        r += chars[Math.floor(Math.random() * chars.length)];
      }
    };
    return r;
  };
  function genStringL(minlength,maxlength) {
    var isIntmin = isInt(minlength);
    var isIntmax = isInt(maxlength);
    if (!isIntmin && !isIntmax) return genString();
    /* one of the two is an integer */
    if (!isIntmin) minlength = 0;
    if (!isIntmax) {
      var l = minlength + genLength();
      return genString(undefined,l);
    };
    /* both, min and max are defined and both are ints */
    if (minlength > maxlength) return genString();
    var l = minlength + genLength();
    if (maxlength < l) {
      return genStringL(minlength,maxlength);
    };
    return genString(undefined,l);
  };
  function genPPropString(pl,p) {
    return pickOrF(pl,p,genString);
  };
  function genObject(exactPropList) {
    var ps = [];
    function addProp(p,r,c) {
      if (p && !(ps.indexOf(p) > -1)) {
        ps.push(p);
        r[p] = c.gen();
      };
    };

    /* If no pl is given, generate a random set of properties,
       which Top Contract */
    if (!exactPropList) exactPropList = genEPL();

    var r = {};
    for (var j in exactPropList) {
      var ep = exactPropList[j];
      if ((ep.name) && (ep.contract)) { // name and contract exists
        // generate a new value for property, and assign it to 
        // the property of the object r.
        addProp(ep.name,r,ep.contract);
      } else {
        // do we have a random property without a name?
        var rand = 0;
        if (isInt(ep.random) && (ep.random > 0)) {
          rand = ep.random;
        } else {
          rand = genLength();
        };
        for (var i = 0; i < ep.random; ++i) {
          var s = genString();
          addProp(s,r,T.TopOUndef);
        };
      };
    };
    return r;
  };
  function genSObject() {
    var eSPL = genSEPL();
    return genObject(eSPL);
  };
  function genPObject(pl,p) {
    if (!p) p = 0.5;
    var pSPL = genEPL(function () { return genPPropString(pl,p); });
    return genObject(pSPL);
  };
  function genObjectWithRegEx(regexPropList) {
    if (!regexPropList) regexPropList = genRPL();
    // TODO
    return {};
  };
  function genArray(contract, length) {
    if (!length) length = genLength();
    if (!contract) contract = T.Top;
    var a = [];
    for (var j = 0; j < length; ++j) {
      a.push(contract.gen());
    };
    return a;
  };
  var genFoU = [genNull,genBoolean,genNumber,genInt,genString,
                genPropString,genSObject,genObject,genArray];
  var genF = [genUndefined,genNull,genBoolean,genNumber,genInt,genString,
              genPropString,genSObject,genObject,genArray];
  function genTop() {
    // reduce probability, that an object or array is
    // created. This ensures Termination. 
    var r = Math.floor(Math.random() * 1000);
    if (r < 2) genF.push(genBoolean);
    if (r < 4) genF.push(genNumber);
    if (r < 6) genF.push(genInt);
    if (r < 8) genF.push(genUndefined);
    if (r < 10) genF.push(genNull);
    if (r < 12) genF.push(genString);
    if (r < 14) genF.push(genPropString);
    return (pick(genF))();
  };
  function genTopOUndef() {
    // reduce probability, that an object or array is
    // created. This ensures Termination. 
    var r = Math.floor(Math.random() * 1000);
    if (r < 2) genFoU.push(genBoolean);
    if (r < 4) genFoU.push(genNumber);
    if (r < 6) genFoU.push(genInt);
    if (r < 8) genFoU.push(genNull);
    if (r < 10) genFoU.push(genString);
    if (r < 12) genFoU.push(genPropString);
    return (pick(genFoU))();
  };
  function initGen() {
    genFoU = [genNull,genBoolean,genNumber,genInt,genString,
              genPropString,genSObject,genObject,genArray];
    genF = [genUndefined,genNull,genBoolean,genNumber,genInt,genString,
            genPropString,genSObject,genObject,genArray];
  };
  /* This generator takes a predicate isValid
   * together with an arbitrary generator (gen),
   * and return a value that fulfills isValid.
   * It may take some time to find a value, such
   * that isValid is fulfilled, so please use
   * this generator only, if the probability,
   * that isValid(gen()) == true is high.
  */
  function restrictTo(isValid,gen) {
    function g() {
      var v = gen();
      if (isValid(v)) return v;
      return g();
    };
    return g;
  };


  /********** shrinking **********/
  // TODO

  /********** utils **********/
  function smem(e,set) {
    for (j in set) {
      if (set[j] === e) {
        return true;
      };
    };
    return false;
  };
  function sadd(e,set) {
    if (smem(e,set)) {
      return set;
    } else {
      var nset = [e];
      for (j in set) {
        nset.push(set[j]);
      };
      return nset;
    };
  }; 
  function max(i1,i2) { if (i1 < i2) return i2; else return i1; };
  function compareArray(a,testArr) {
   if (a.length != testArr.length) return false;
    for (var i = 0; i < testArr.length; i++) {
        if (a[i].compare) { 
            if (!a[i].compare(testArr[i])) return false;
        }
        if (a[i] !== testArr[i]) return false;
    }
    return true;
  };
  function unSafePick(a) {
    return a[Math.round(Math.random() * (a.length-1))];
  };
  function pick(a) {
    if (isSArray(a)) {
      return a[Math.round(Math.random() * (a.length-1))];
    } else {
      throw "Call pick with an array";
    };
  };
  function pickOrF(a,p,f) {
    if (isSArray(a) && p && (p >= 0) && (p <= 1) && (Math.random() < p)) {
      return pick(a);
    } else {
      return f();
    };
  };

  function getFun(f1,f2) {
    if (isFunction(f1)) {
      return f1;
    } else {
      return f2;
    };
  };

  function singleQuote(param) {
	return "'" + param + "'";
  };
  
  function concat(a,sep,left,right,showProp,showValues,nextLine) {
	var numberOfProp = 0;
	var s = "";
	var sepWithNL = sep;
	if (nextLine && showProp) sepWithNL += nextLine();
    for (var i in a) 
	{
      s += sepWithNL;
	  numberOfProp++; 
	  if (showProp) s += i + ": ";
      if (showValues) {
        s += valueToString(a[i],nextLine);
      } else {
        s += a[i];
      };
	}
	if (numberOfProp > 1) return left + s.substring(sep.length) + right ;
	else return left + s.substring(sepWithNL.length) + right;
  };

  function valueToString(v,nextLine) {
	var modnextLine = undefined;
	if (nextLine) 
	{
		modnextLine = 
			function () {
				return nextLine() + "&nbsp;&nbsp;&nbsp;"; 
			};
	};
	if (v === null) return 'null';
    if (v === undefined) return 'udf';
    switch (typeof(v)) {
    case 'string': 
      return singleQuote(v);
    case 'object':
      if (isSArray(v)) {
        return concat(v,",",'[',']',false,true,modnextLine);
      } else {
        if (v.getcdes) {
          return v.getcdes();
        } else {
          return concat(v,",",'{','}',true,true,modnextLine);
        };
      }
    case 'function':
      return "" + v;
    default:
      return "" + v;
    };
  };

  var testContracts = 0;
  var verifyContracts = 0;
  var testCount = 0;
  var failCount = 0;
  var errorContract = 0;
  var wellTestedCount = 0;
  /********** test interface **********/
  
  function run(afterRun) {
    function logCExp() {
      logCounterExpStart();
      for (var m in counterexp) {
        var cm = counterexp[m];
        for (var i in cm) {
          logCounterExp(cm[i]);
        };
      };
      logStat();
    };

    initialize();
    var toDoM = [];
    var toDoT = [];
    var toDoMax = 0;
    var toDoCount = 0;
    var test;
    var anz = 0;
    for (var m in tests) {
      toDoM.push({mname: m, m: tests[m]});
    };
    toDoM.reverse();

    /* run handler */
    function min(a,b) {
      if (a < b) return a; else return b;
    };
    function runChecks() {
      var t = min(toDoCount + testCountPerStep,toDoMax);
      try {
        var b = true;
        for (; toDoCount < t; ++toDoCount) {
          initGen();
          /* this notation results in searching one counterexample
             per each contract, and then stop */
          ++testCount;
          b = b && test.c.check(test.v);
          if (!b) {
            var ce = test.c.getCExp();
            if (ce.isCExp && (ce.isCExp())) {
              collectCounterExample(ce);
            };
            break;
          };
          /* Use this notation to continue searching counterexamples
             for a contract, even if an other counterexampe is found. */
          //b = c.check(v) && b;
        };
        
        if (!b) {
          ++failCount;
          toDoMax = toDoCount;
        } else {
          if (toDoCount >= toDoMax) {
            ++wellTestedCount;
          };
        };
        if (toDoCount >= toDoMax) {
          logTest(test.v,test.c,b,toDoCount);
        };
      } catch(e) {
        var params = test.c.get_last_created_values();
        ++errorContract;
        logError(e,test.c,params);
        toDoMax = toDoCount;
      };
      resetMarked();
      logStat();
    };
    function runTest() {
      test = toDoT.pop();
      ++testContracts;
      try {
        if (test.c.genNeeded && (test.c.genNeeded(test.v))) {
          toDoCount = 0;
          toDoMax = test.t;
          return ;
        } else {
          /* check contract without test it */
          toDoCount = 0; toDoMax = 0;
          var b = test.c.check(test.v);
          if (b) {
            ++verifyContracts;
          } else {
            ++failCount;
          };
          logTest(test.v,test.c,b);          
        };
      } catch (e) {
        var params = test.c.get_last_created_values();
        ++errorContract;
        logError(e,test.c,params);
        toDoMax = toDoCount;
      };
    };    
    /* select next modul */
    function runModul() {
      var modul = toDoM.pop();
      var m = modul.mname;
      logModuleChange(m);
      var tm = modul.m;
      for (var i in tm) {
        toDoT.push(tm[i]);
      };
    };
    function resetAM() {
      if (cancelAM) {
        logTest(test.v,test.c,true,toDoCount);          
        cancelAC = false;
        cancelAM = false;
        toDoT = [];
        toDoCount = 0;
        toDoMax = 0;
        return true;
      } else {
        return false;
      };             
    };
    function resetAC() {
      if (cancelAC) {
        logTest(test.v,test.c,true,toDoCount);          
        cancelAC = false;
        toDoCount = 0;
        toDoMax = 0;
        return true;
      } else {
        return false;
      };
    };
    /* function which is called in regular intervalls */
    var cancel = false;
    var cancelAC = false;
    var cancelAM = false;
    doCancel = function() { cancel = true; };
    doCancelAC = function() { cancelAC = true; };
    doCancelAM = function() { cancelAM = true; };
    (function() {
      logStat();
      if (cancel) { return; };
      if ((toDoM.length < 1) && (toDoT.length < 1) && (toDoCount >= toDoMax)) {
        logCExp();
	if (afterRun) afterRun();
      } else {
        setTimeout(arguments.callee, 0);
        if (resetAM()) return;
        if (resetAC()) return;
        if ((toDoCount < toDoMax) && (!cancelAC)) {
          return runChecks();
        };
        if ((toDoT.length > 0) && (!cancelAM)) {
          return runTest();
        } else {
          runModul();
        };
      };
    })();    
  };
  var doCancel, doCancelAM, doCancelAC;

  function add(m,v,c,t) {
    if (!(tests[m])) {
      tests[m]= [];
    };
    tests[m].push({v: v, c: c, t: t});
  };
  function collectCounterExample(ce) {
    if (!(counterexp[module])) {
      counterexp[module] = [];
    };
    var cem = counterexp[module];
    for (var i in cem) {
      if (cem[i].compare(ce)) {
        return;
      };
    }
    counterexp[module].push(ce);
  };

  /********** logger **********/
  function logFail(v,c,anz) {
    for (l in logger) {
      if (logger[l].fail) logger[l].fail(v,c,anz);
    };
  };
  function logSuccess(v,c,anz) {
    for (l in logger) {
      if (logger[l].success) logger[l].success(v,c,anz);
    }
  };
  function logError(e,c,params) {
    for (l in logger) {
      if (logger[l].error) logger[l].error(e,c,params);
    }
  };
  function logTest(v,c,test,anz) {
    if (test) {
      logSuccess(v,c,anz);
    } else {
      logFail(v,c,anz);
    };
  };
  function logModuleChange(m) {
    module = m;
    for (l in logger) {
      if (logger[l].moduleChange) logger[l].moduleChange(m);
    };
  };
  function logCounterExpStart() {
    for (l in logger) {
      if (logger[l].CExpStart) logger[l].CExpStart();
    };
  };
  function logCounterExp(ce) {
    for (l in logger) {
      if (logger[l].CExp) logger[l].CExp(ce);
    };
  };
  function logStat() {
    for (l in logger) {
      if (logger[l].stat) 
        if (logger[l].stat) 
          logger[l].stat(testContracts,testCount,failCount,verifyContracts,errorContract,wellTestedCount);
    };
  };
  function logAssertParam(cl,pl,str) {
    for (l in logger) {
      if (logger[l].assertParam) logger[l].assertParam(cl,pl,str);
    };
  };
  function logAssertReturn(c,pv) {
    for (l in logger) {
      if (logger[l].assertReturn) logger[l].assertReturn(c,v);
    };
  };
  function registerLogger(o) {
    logger.push(o);
  };

  /********** contracts **********/
  function newContract(contractType,check_var,generate,getcdes,setcdes,
                       genNeeded,initcdes,simplValue) {
    var ct = makeContractType(contractType);
    var ce = undefined;
    var cdes = initcdes;
    var gen = getFun(generate,function() { return generate; });
    var check = getFun(check_var, 
                       function(v) { 
                         if (check_var === v) { 
                           return true; 
                         } else { 
                           return false;
                         }; 
                       });
    var sv = getFun(simplValue,function(v) { return v; });
    this.check = function() {
      return check.apply(this,arguments);
    };
    this.gen = function() {
      var g = gen.apply(this,arguments);
      var args = [];
      args.push(g);
      for (var i=0;i<arguments.length;i++) {
        args.push(arguments[i]);
      }
      if (this.check.apply(this,args)) {
        return g;
      } else {
        throw ("Implemenation of Generator is not valid. Please ensure "
               + "that each value that is generated by the "
               + "do_generate function passes the do_check function.");
      };
    };
    this.simpl = function(v) {
      var r = sv.call(this,v);
    };
    this.failToString = function(v) {
      var r = ("Contract '" + this.getcdes()
               + "' is <em>not</em> fulfilled by the value: "
               + v + ".")
      return r;
    };
    this.okToString = function(v) {
      var r = "Contract '" + this.getcdes() + "' is fulfilled by value: " + v + ".";
      return r;
    };
    this.getcdes = getFun(getcdes,function() { return cdes; });
    this.setcdes = getFun(setcdes,function(s) { cdes = s; });
    this.genNeeded = getFun(genNeeded,function (v) { return false; });
    this.getCExp = function() { return ce; };
    this.registerCExp = function(setce) { ce = setce };
    this.toString = this.getcdes;
    this.getContractType = function() { return ct; };
  };
  function newSContract(check,generate,cdes,ct,genNeeded) {
    return newContract.call(this,ct,check,generate,null,null,genNeeded,cdes);
  };
  function newPContract(ct,check,pl,p,gen,cdes,genNeeded) {
    if (!p) p = 0.5;
    if (isSArray(pl)) {
      return newContract.call(this,ct,check,
                              function() { return pickOrF(pl,p,gen); },
                              null,null,
                              genNeeded,
                              cdes);
    } else {
      throw "PContract needs array as parameter";
    };
  };
  function newSingletonContract(value,cdes,genNeeded) {
    return newContract.call(this,ctBasic,value,value,null,null,genNeeded,cdes);
  };

  /* Interface of counterexamples : {
       isCExp: void -> true;
       value: value;
       contract: contract;
       ? (params: value array; )
       ? (result: value; )
       module: string;
       valueToString: void -> string;
       contrToString: void -> string;
       paramToString: void -> string;
       resultToString: void -> string;
       moduleToString: void -> string;
     }
  */
  function newCExp(value,contract,params,result, module) {
    function contrToString(nextLine) {
      return contract.getcdes();
    };
    function paramToString(nextLine) {
      return valueToString(params,nextLine );
    };
    function resultToString(nextLine) {
      return valueToString(result,nextLine);
    };
    this.compare = function(that) {
      return ((this.value == that.value)
              && (this.contract === that.contract)
              && compareArray(this.params, that.params)
              && (this.result == that.result));
    };
    this.isCExp = function() { return true; };
    this.value = value;
    this.contract = contract;
    this.params = params;
    this.result = result;
    this.module = module;
    this.valueToString = function() { return valueToString(value); };
    this.contrToString = contrToString;
    this.paramToString = paramToString;
    this.resultToString = resultToString;    
    this.moduleToString = function() { return module; };
  };
  function newCExpUnion(c,ce1,ce2,module) {
    this.isCExp = function() { return true; };
    this.value = c1.value;
    this.contract = c;
    this.valueToString = function () { return valueToString(ce1.value) };
    this.contrToString = function () { return (c.getcdes()); };
    this.paramToString = function () {
      return ("first: " + ce1.paramToString() +
              "second: " + ce2.paramToString());
    };
    this.resultToString = function () {
      return ("first: " + ce1.resultToString() + 
              "second: " + ce2.resultToString());
    };
    this.module = module;
    this.moduleToString = function() { return module; };
  };

  /* is called in the run method */ 
  var initialize = 
    (function() {
      var init = false;
      return (function () {
                if (init) return;
                /* compute maxint */
                if (!(MAXINT)) {
                  var y = 1;
                  var x = 0;
                  for (var i = 0; i<31; i++) {
                    x = x + y;
                    y = y * 2;
                  };
                  MAXINT = x;
                };
              });
    })();

 /** Logger Interface: 
     { fail: (value, contract) -> void;
       success: (value, contract, anz) -> void;
       error: string -> void;
       moduleChange: string -> void;
       CExpStart: void -> void;
       CExp: CExp -> void;
       stat: (totalCNumber, totalTestNumber, failed, verified,
              errors, wellTested) -> void;
     }
 */
  function createDivLogger(divId) {
    function app(s) {
      var divLog = document.getElementById(divId);
      var pTag = document.createElement("p");
      pTag.innerHTML = s;
      divLog.appendChild(pTag);
    };
    var o = 
      { fail: function (v,c) {
          var s = c.failToString(v);
          if (!app) {
            app(s);
          } else {
            app(s + " Tests run: " + anz);
          };
        },
        success: function (v,c,anz) {
          if (!anz) {
            var s = c.okToString(v);
            app(s);
          } else {
            app(c.okToString(v) + " Tests run: <em>" + anz + "</em>");
          }
        },
        error: app,
        moduleChange: function(m) {
          var divLog = document.getElementById(divId);
          var h2 = document.createElement("h2");
          h2.innerHTML = m;
          divLog.appendChild(h2);
        }
      };
    return o;
  };

  function createWebLogger(page_name,send_complete) {
    var failed = 0;
    var errors = 0;
    var tested = 0;
    
    var o = {};
    o.stat = function(tCN,tTN,f,v,e,wT) {
      failed = f;
      errors = e;
      tested = wT;
    };
    o.send_stat = function() {
      function getHttpRequest(url) { 
        var xmlhttp = null; 
        // Mozilla 
        if (window.XMLHttpRequest) { 
          xmlhttp = new XMLHttpRequest(); 
        } // IE 
        else if (window.ActiveXObject) { 
          xmlhttp = new ActiveXObject("Microsoft.XMLHTTP"); 
        } 
        xmlhttp.open("GET", url, true); 
        xmlhttp.onreadystatechange = function() { 
          if (send_complete) send_complete();
        } 
        xmlhttp.send(null); 
      }; 
      getHttpRequest(page_name 
                     + '?failed=' + failed 
                     + '&errors=' + errors 
                     + '&tested=' + tested
                     + '&fn=' + filename);
    };
    return o;
  };
  
 /** Logger Interface: 
     { fail: (value, contract) -> void;
       success: (value, contract, anz) -> void;
       error: string -> void;
       moduleChange: string -> void;
       CExpStart: void -> void;
       CExp: CExp -> void;
       stat: (totalCNumber, totalTestNumber, failed, verified,
              errors, wellTested) -> void;
     }
 */
  function createEnumLogger(divId, enType) {
    var enCId = "";
    function app(s) {
      var enLog = document.getElementById(enCId);
      var item = document.createElement("li");
      item.innerHTML = s;
      enLog.appendChild(item);
    };
    function newDef(dl,s1,s2) {
      setDef(dl,false,s1,s2);
    };
    function setDef(dl,id,s1,s2) {
      var n2;
      if (id) { 
        n2 = document.getElementById(id); 
      };
      if (!n2) {
        var n = document.createElement("dt");
        n.innerHTML = s1;
        dl.appendChild(n);
        var n2 = document.createElement("dd");
        n2.setAttribute('id',id);
        dl.appendChild(n2);
      };
      n2.innerHTML = s2;
    };
    function aCE(ce) {
      var enLog = document.getElementById(enCId);
      var item = document.createElement("li");
      enLog.appendChild(item);
      var dl = document.createElement("dl");
      item.appendChild(dl);
      newDef(dl,"value",ce.valueToString(nextLine));
      newDef(dl,"contract",ce.contrToString());
      newDef(dl,"parameter",ce.paramToString(nextLine));
      newDef(dl,"result", ce.resultToString());
      // newDef(dl,"expected",ce.resultExpToString());
    };
	
	function nextLine() {
      return "<br/>";
	};
	
    function newHeadding(s,m) {
      var divLog = document.getElementById(divId);
      var h2 = document.createElement("h2");
      h2.innerHTML = s;
      divLog.appendChild(h2);
      var en = document.createElement(enType);
      enCId = divId + m;
      en.id = enCId;
      divLog.appendChild(en);
    };
    function newDivHeading(title,c) {
      var div = document.createElement("div");
      var h = document.createElement("h2");
      div.appendChild(h);
      h.innerHTML = title;
      if (c) div.setAttribute('class',c);
      return div;
    };
    function newB(title,onclick) {
      var b = document.createElement('button');
      b.innerHTML = title;
      b.onclick = onclick;
      return b;
    };
    var log_console;
    var initStat = 
      (function () {
        var init = false;
        return function () {
          if (!init) {
            var divLog = document.getElementById(divId);
            var d = document.createElement('div');
            var fc = divLog.firstChild;
            divLog.insertBefore(d,fc);
            d.setAttribute('id','first');
            
            var stat = newDivHeading("Statistic",'clear');
            d.appendChild(stat);
            var w = newDivHeading("Warnings",'right');
            d.appendChild(w);
            code = document.createElement('code');
            w.appendChild(code);
            log_console = code;

            var dl = document.createElement("dl");
            stat.appendChild(dl);
            dl.setAttribute('id',divId + '_dl');
            stat.appendChild(newB('cancel all',doCancel));
            stat.appendChild(newB("cancel active contract",doCancelAC));
            stat.appendChild(newB("cancel active modul",doCancelAM));

            init = true;
          };
        };
      })();
    function statistic(totalCNumber,totalTestNumber,failed,verified,errors,wellTested) {
      initStat();
      var dl = document.getElementById(divId + '_dl');
      setDef(dl,'NoC',"Number of contracts: ", totalCNumber);
      setDef(dl,'NovC',"Number of verified contracts: ", verified);
      setDef(dl,'NofC',"Number of failed contracts: ", failed);
      setDef(dl,'Noce',"Number of contracts where check exists with an error: ", errors);
      setDef(dl,'Notc',"Number of well tested contracts: ", wellTested);
      setDef(dl,'Not',"Number of tests run: ", totalTestNumber);
    };
    var aP = 0;
    function assertParam(cl,pl,str) {
      aP++;
      if (aP < 10) {
        log_console.innerHTML = 
          log_console.innerHTML + 
          str + ": " +
          "Parameters passed to function not valid: " + cl + ", " + valueToString(pl) + "<br/>\n";
      };
    };
    var aR = 0;
    function assertReturn(cl,v) {
      aR++;
      if (aR < 10) { 
        log_console.innerHTML = log_console.innerHTML + 
          "Return value of " + cl + " not valid: " + valueToString(v); 
      };
    };
      var o = 
      { fail: function (v,c,anz) {
          if (!anz) {
            var s = c.failToString(v);
            app(s);
          } else {
            var s = c.failToString(v);
            app(s + " Tests run: " + anz);
          };
        },
        success: function (v,c,anz) {
          if (!anz) {
            var s = c.okToString(v);
            app(s);
          } else {
            app(c.okToString(v) + " Tests run: <em>" + anz + "</em>");
          }
        },
        error: function (e,c,params) { 
          app("While testing contract " + c.getcdes() + ", an error happens: " + e);
          app("The parameters passed to the function were: " + params);
        },
        moduleChange: function (m) { newHeadding("Module: " + m, m); },
        CExpStart: function() { newHeadding("Collected Counterexamples", "cex"); },
        CExp: function(ce) { aCE(ce) },
        stat: statistic,
        assertParam: assertParam
      };
    return o;
  };

/********** Null, Undefined, Boolean, String, Number **********/
T.Top = new newSContract(function() { return true; }, genTop, "top");
T.TopOUndef = new newSContract(function(v) { return (v !== undefined); }, genTopOUndef, "utop");
T.PTop = function(a,p) { 
  return newPContract(ctNoType,function() { return true; }, a, p, genTop, "top"); 
};
T.Null = new newSingletonContract(null, "null");
T.Undefined = new newSingletonContract(undefined, "undefined");
T.Boolean = new newSContract(isBoolean,genBoolean,"boolean",ctBasic);
T.True = new newSingletonContract(true, 'true');
T.False = new newSingletonContract(true, 'false');
T.String = new newSContract(isString, genString, 'string',ctBasic);
T.Number = new newSContract(isNumber,genNumber,"number",ctBasic);
T.Integer = new newSContract(isInt,genInt,"integer",ctBasic);
T.PInteger = function (iA,p) {
  return new newPContract(ctBasic,isInt,iA,p,genInt,"PInteger{" + valueToString(iA) + "}");
};
T.AInteger = function(iList,fList) {
  if (!iList) iList = [0,1];
  iList = sadd(0,iList);
  iList = sadd(1,iList);
  if (!fList) fList = T.ABasicFuns;
  return new newSContract(isInt,function() { return genAInt(iList,fList); } ,
                          "AInteger{" + valueToString(iList) + "; " + valueToString(fList) + "}",
                          ctBasic);
};
T.Id = new newSContract(function (x,y) { return x === y; },
                        function (x) { return x; },
                        "Id",
                        ctBasic);
T.ABasicFuns = [{getcdes: function() { return "+"; }, arity: 2, f: function (x,y) { return x + y; }},
                {getcdes: function() { return "-"; }, arity: 2, f: function (x,y) { return x - y; }},
                {getcdes: function() { return "*"; }, arity: 2, f: function (x,y) { return x * y; }},
                {getcdes: function() { return "/"; }, arity: 2, f: function (x,y) { return x / y; }}
               ];
T.IIntervall = function(low,high) {
  if (isInt(low) && isInt(high)) {
    var o = new newSContract(function(v) { return isIInt(low,high,v); },
                             function() { return genIInt(low,high); },
                             "[" + low + "..." + high + "]",
                             ctBasic);
    return o;
  } else {
    if ((!isNumber(low)) || (!isNumber(high))) {
      throw "Intervall needs numbers as bounds";
    } else {
      throw "An Integer Intervall needs Integers as bounds, not floats.";
    };
  };
};
T.NIntervall = function(low,high) {
  if (isNumber(low) && isNumber(high)) {
    var o = new newSContract(function (v) { return isNInt(low,high,v); },
                             function() { return genNInt(low,high); },
                             "[/" + low + "..." + high + "]",
                             ctBasic);
    return o;
  } else {
    throw "Invervall needs number as bounds";
  }
};

/********** Objects **********/
// Object without additional informations 
T.Object = new newSContract(isObject,genObject,"object",ctObject);
/* Object with properties, that are simple. Property names 
 * only contains characters and digits. 
 */ 
T.SObject = new newSContract(isObject,genSObject,"sobject",ctObject);
/* Object like "Object", but with additional information about
 * properties that are important. 
 * If a property name is generated randomly, p is the probability
 * that a name from the list pl of property names is choose. 
 * If no name is choose from pl, a simple property, only containing
 * characters and digits is generated. 
 */
T.PObject = function(pl,p) {
  return new newSContract(isObject,
                          function() { return genPObject(pl,p); },
                          "pobject{" + valueToString(pl) + "}",
                          ctObject);
};
/* Objects with an exact property list. pl is a list of objects 
 * which contains property names (as name) and contracts (as contract).
 * The checker ensures that each property given by pl exists and that
 * the value of the property fulfills the suitable contract. 
 * The generator creates objects randomly with exactly the given set
 * of properties, calling gen() for each contract given in pl for each
 * property. 
 */
T.EObject = function(pl) {
  var o = new newContract(ctObject,
                          function (v) { return isObject(v,pl); },
                          function () { return genObject(pl); },
                          function () { 
                            var s = "eobject{";
                            var pls = [];
                            var random = false;
                            for (j in pl) {
                              var p = pl[j];
                              if (p.name && p.contract) {
                                pls.push(p.name + ":" + p.contract.getcdes());
                              } else {
                                if (p.random) { 
                                  random = true;
                                } else { 
                                  pls.push(valueToString(p));
                                };
                              };
                            };
                            s += concat(pls,",","","",false);
                            if (random) {
                              s += ",...";
                            };
                            return s + "}";
                          });
  return o;
};
/* An Array. t is the type of the elements. */
T.Array = function(t) { 
  return (new newContract(ctArray,
                          function (v) { return isArray(v,t); }, 
                          function () { return genArray(t); },
                          function () { return "array[" + t.getcdes() + "]"; } ));
}

/*********** FUNCTION **********/
/* A function contract. pl is a list of contracts for the
 * parameters of the function, while rt describes the result
 * value of the function. 
 * T.Function([T.Boolean,T.String], T.String) states:
 *   (boolean, string) -> string
 * The check for a function is done by generating a value for
 * each parameter, then call the function and checking, if the
 * result value fulfills rt.
 */
T.Function = function(pl,rt) {
  var lcvs;
  var pldes = "";
  for (var i in pl) {
    if (i > 0) pldes += ", ";
    pldes += pl[i].getcdes();
  };
  function check(v) {
    var t = typeof(v);
    if (t !== 'function') return false;
    var pvl = [];
    for (var i in pl) {
      pvl[i] = pl[i].gen();
    };
    lcvs =  valueToString(pvl);
    var res = v.apply(null,pvl);
    var cres = rt.check(res);
    if (!cres) {
      /* collect counterexample */
      this.registerCExp(new newCExp(v,this,pvl,res));
      return false;
    } else {
      return true;
    };
  };
  function getcdes() {
    return pldes + "->" + rt.getcdes();
  };
  function setcdes() {
    throw "Setting description for function contract not supported";
  };
  var c = new newContract(ctFunction,check,gen,getcdes,setcdes,isFunction);
  function gen() {
    return function () {
      if (c.checkParams(arguments)) return rt.gen();
      
      // TODO: What should happen, if the arguments 
      // did not pass the check?
    };
  };
  c.checkParams = function (plv) {
    for (var i in pl) {
      v = plv[i];
      c = pl[i];
      if (!(c.check(v))) {
        return false;
      }
    };
    return true;
  };
  c.checkReturn = function(v) {
    var ok = rt.check(v);
    if (!ok) {
      logAssertReturn(c,v);
    };
  };
  c.get_last_created_values = function() {
    return lcvs;
  };
  return c;
};

T.newDepend = function(order,dl) {
  var dparam = {};
  function getDepend(i) {
    if (i < dl.length - 1) return dl[i];
  };
  function getDependResult() {
    return dl[dl.length - 1];
  };
  function getOrder() {
    return order;
  };
  dparam.getDepend = getDepend;
  dparam.getDependResult = getDependResult;
  dparam.getOrder = getOrder;
  return dparam;
}
function newdValues () {
  var scope = [[]];
  var as = 0;
  this.getValue = function(s,p) {
    return scope[s-1][p-1];
  };
  this.setValue = function(param,value) {
    scope[as][param] = value;
  };
};
T.DFunction = function(pl,rt,dparam) {
  var lsvs = "";
  var pldes = "";
  for (var i in pl) {
    if (i > 0) pldes += ", ";
    pldes += pl[i].getcdes();
  };
  function getValues(dvalues,dpl) {
    var dvl = [];
    for (var i in dpl) {
      dvl.push(dvalues.getValue.apply(dvalues,dpl[i]));
    };
    return dvl;
  };
  function check(v,dvalues) {
    var t = typeof(v);
    if (t !== 'function') return false;
    if (dvalues === undefined) dvalues = new newdValues();
    var pvl = [];

    var order = dparam.getOrder();
    for (var i in order) {
      /* index of parameter, that should be generated */
      var p = order[i];
      
      /* list of ($,anz) tuppels, from which the parameter depends */
      var dpl = dparam.getDepend(p);

      /* collected values, corresponding to the ($,anz) list */
      var dvl = getValues(dvalues,dpl);

      /* call the generator, this = pl[p], other parameters dvl */
      var value = pl[p].gen.apply(pl[p],dvl);
      pvl[p] = value;

      dvalues.setValue(p,value);
    }
    lcvs =  valueToString(pvl);
    var res = v.apply(null,pvl);
    var cres = rt.check(res);
    if (!cres) {
      /* collect counterexample */
      this.registerCExp(new newCExp(v,this,pvl,res));
      return false;
    } else {
      return true;
    };
  };
  function getcdes() {
    return pldes + "-D>" + rt.getcdes();
  };
  function setcdes() {
    throw "Setting description for function contract not supported";
  };
  var c = new newContract(ctFunction,check,{},getcdes,setcdes,isFunction);
  c.checkParams = function (plv) {
    for (var i in pl) {
      v = plv[i];
      c = pl[i];
      if (!(c.check(v))) {
        return false;
      }
    };
    return true;
  };
  c.checkReturn = function(v) {
    var ok = rt.check(v);
    if (!ok) {
      logAssertReturn(c,v);
    };
  };
  c.get_last_created_values = function() {
    return lcvs;
  };
  return c;
};



/*********** UNION **********/
 var Union, UnionAddSimplRule;
(function () {
  var simplRules = [];
  function addSimpl(sr) {
    if (isFunction(sr)) {
      simplRules.push(sr);
    }
  };
  function createUnion(c1,c2) {
    function check(v) {
      var c1r = c1.check(v);
      if (c1r) {
        return true;  
      } else {
        var c2r = c2.check(v);
        if (!c2r) {
          // TODO: Is this the intended semantics?
          var ce1 = c1.getCExp();
          var ce2 = c2.getCExp();
          if (ce1 && ce2) {
            this.registerCExp(new newCExpUnion(this,ce1,ce2));
          } else {
            if (ce1) {
              this.registerCExp(ce1);
            } else {
              if (ce2) {
                this.registerCExp(ce2);
              };
            };
          };
          return false;
        } else {
          return true;
        };
      }
    };
    function generate() {
      var r = Math.random();
      if (r < 0.5) {
        return c1.gen();
      } else {
        return c2.gen();
      }
    };
    function getcdes() {
      return ("(" + c1.getcdes() + " or " + c2.getcdes() + ")");
    };
    for (var i in simplRules) {
      var sr = simplRules[i];
      var c = sr(c1,c2);
      if (c) return c;
    };
    return new newContract(ctComplex,check,generate,getcdes);
  };
  Union =  createUnion;
  UnionAddSimplRule = addSimpl;
  
  function simplTrueFalseBool(c1,c2) {
    if (((c1 === T.True) && (c2 === T.Boolean))
        || ((c1 === T.Boolean) && (c2 === T.True))
        || ((c1 === T.False) && (c2 === T.Boolean))
        || ((c1 === T.Boolean) && (c2 === T.False))) {
      return T.Boolean;
    } else {
      return false;
    };
  };
  function simplIntervall(c1,c2) {
    

  };

  addSimpl(simplTrueFalseBool);
})();


/********** Intersection **********/
T.Intersection = function(c1,c2) {
  if (c1.getContractType 
      && (c1.getContractType() == ctFunction)
      && c2.getContractType 
      && (c2.getContractType() == ctFunction)) {
    
  } else {
    throw "Intersections are only allowed for two function contracts";
  }
};

/********** NAMES **********/
 var resetMarked, Name, Let;
(function () {
  var ntable = {};
  var cnames = [];
  var testTable = function(name,f,g,err) {
    var c = ntable[name];
    if (c) {
      if (!(c.marked)) {
        c.marked = true;
        var r = f(c.contract);
        c.marked = false;
        return r;
      } else {
        return g(c.contract);
      }
    } else {
      throw ("Invalid contract! There exists no contract with name: '" 
             + name + "'. " + err);
    };

  };
  Name = function(name) {
    var gcdes = function(i) {
      return "Name: " + name + ", Image: " + i;
    };
    function throwRecError(cstr) {
      throw ("Invalid Contract '"
             + cstr 
             + "'! Recursion, but no function contract visted");
    };
    function check(v) {
        var r = testTable(name,
                          function(c) { var tmp = c.check(v); return tmp; },
                          function(c) { throwRecError(gcdes(c.getcdes())); },
                          "Invalid call of check.");
        return r;
    };
    function generate() {
        var r = testTable(name,
                          function(c) { var tmp = c.gen(); return tmp; },
                          function(c) { throwRecError(gcdes(c.getcdes())); },
                          "Invalid call of generate.");

        return r;
    };
    function getcdes() {
      var r = testTable(name,
                        function(c) { var tmp = c.getcdes(); return gcdes(tmp); },
                        function(c) { return "Name: " + name },
                        "Name: " + name + "(no Image)");
      return r;
    };
    var o = new newContract(ctName,check,generate,getcdes);
    cnames.push({name: name, contract: o, marked: false});
    return o;
  };

  Let = function (name,c) {
    ntable[name] = {name: name, contract: c};
  };
  resetMarked = function() {
    for (var i in ntable) {
      var o = ntable[i];
      o.marked = false;
    }
  };
})();

 
/**********************************/
/*            Interface           */
/**********************************/
 /* Logger */
 T.registerLogger = registerLogger;
 /** Logger Interface: 
     { fail: (value, contract) -> void;
       success: (value, contract, anz) -> void;
       error: string -> void;
       moduleChange: string -> void;
       CExpStart: void -> void;
       CExp: CExp -> void;
       stat: (totalCNumber, totalTestNumber, failed, verified,
              errors, wellTested) -> void;
       assertParam: ;
>      assertReturn: ;              
     }
 */
 T.createEnumLogger = createEnumLogger;
 T.createWebLogger = createWebLogger;

 /* Contract */
 /** Contract Interface:
     { Checks if the parameter fulfills the value.
       * check: value -> boolean;
       
       generate parameters for functions
       * gen: void -> vlaue;

       simplify counterexamples
       * simpl: value -> value;

       Tests if we need to generate value to check function
       This will be true if value is a function, or an
       object that does provide methods. Otherwise we can
       do the check without generating examples.
       * genNeeded : value -> boolean

       Returns the counterexample that breaks the contraint
       if a test find a counterexample.        
       * getCExp: void -> CExp
       
       Register a counterexample for a contract
       * registerCExp: CExp -> void

       String methods, to generate logging infos
       * failToString: value -> string;
       * okToString: value -> string;
       * getcdes: void -> string;
       * setcdes: string -> void;
       * toString: void -> string;
     }
 */
 T.Name = Name;
 T.Let = Let;
 T.Union = Union;

 /** TODO: needs type signature and docu */
 T.newContract = newContract;

 /* Tests */
 T.run = run;
 T.add = add;
 T.setStepCount = function(ns) { testCountPerStep = ns; };

 /** newSContract: (check, generate, cdes, ctype, genNeeded) -> Contract
     The function newSContract creates a new Contract. It 
     takes 5 parameters, but you can omit the last two 
     of them.
     check is a predicate for the contract,
     generate is a generator and 
     cdes is a string representation for a contract.
 */
 T.newSContract = newSContract;

 /* Checks */
 T.isNull = isNull;
 T.isTrue = isTrue;
 T.isFalse = isFalse;
 T.isBoolean = isBoolean;
 T.isUndefined = isUndefined;
 T.isNumber = isNumber;
 T.isInt = isInt;
 T.isIInt = isIInt;
 T.isNInt = isNInt;
 T.isString = T.isString;
 T.isFunction = isFunction;
 T.isArray = isArray;
 T.isObject = isObject;

 /********* Generaters *********/
 T.genNull = genNull;
 T.genUndefined = genUndefined;
 T.genBoolean = genBoolean;
 T.genString = genString;
 T.genStringL = genStringL;
 T.genInt = genInt;
 /* genIInt: (int,int) -> int
    genIInt(low,high) is a uniform random genrator 
    over the integer intervall [low,high]. 
 */
 T.genIInt = genIInt;
 T.genNInt = genNInt;

 /* genNumber: void -> float
    genNumber generates a random float.
 */
 T.genNumber = genNumber;
 /* genLength : void -> int
    genLength() is a random generator that is used
    to randomly decide how long a string should be, 
    or how many properties an object should have.
    It generates positive integer value, while
    i has the probability 1/2^i.
 */
 T.genLength = genLength;
 /* genAInt : ( [int],[op], float ) -> int
    genAInt(iList,fList,p):
    iList: is a list of integers that are used as leafs 
           of the tree
    fList: is a list of operators which has a arity, a
           string representation and a function that 
           evaluates the opertor on two integers.
    p:     is the probability, that a leaf is picked, and
           no operator is choosen.
 */
 T.genAInt = genAInt;

 /* genTree : (value -> bool, [value], [op], float, bool) -> value 
    genTree(pred,leafList,nodeList,p,functions):
    pred:      Is the predicate that determins, if a generated tree is valid
    leafList:  Either a list of leafs or a list of functions that generate
               trees (depending on the value of functions).
    nodeList:  A list of functions that allows combining a number of 
               already generated trees. 
               { arity: int, f: (v1,...,v_arity) -> value }
    p:         Probability, that a leaf is picked, and no operator is choosen
    functions: Determens if leafList is a list of functions or values.    
 */
 T.genTree = genTree;

 /* restrictTo: (Top -> bool, void -> value) -> value
    the call restrictTo(isA,genB) is a generator for
    value of the set A, if genB generates all values
    from B, and it holds A \subset B, and the probability
    is greater than zero. The higher the probablity is,
    the faster the new generator will work.
 */
 T.restrictTo = restrictTo;

 /* genEPL : (void -> string) -> [{name: string, contract: contract}]
    genEPL takes a function which generates property names,
    and randomly generate a list of objects, which themselfs
    saves the generated name and a suiteable contract for
    this property.
    Use the array of objects to create an object call gen from 
    the contract to generate suiteable values for each property.
    If the parameter is omited, genString is used. 
 */
 T.genEPL = genEPL;

 (function () {
   var vars = {};
   T.setVar = function(vname,value) {
     vars[vname] = value;
   };
   T.getVar = function(vname) {
     return vars[vname];
   };
   T.pushVar = function(vname,value) {
     if (!isSArray(vars[vname])) {
       vars[vname] = [];
     };
     vars[vname].push(value);
   };
   T.popVar = function(vname) {
     if (!isSArray(vars[vname])) {
       // something strange happens...
     } else {
       return vars[vname].pop();
     };
   };
 })();

 var testmode =
   (function () {
     var inTestMode = false;
     function intoTest() { inTestMode = true };
     function leafTest() { inTestMode = false };
     function getTestMode() { return inTestMode; };
     return { iT: intoTest, lT: leafTest, gT: getTestMode };
   })();
 var intoTest = testmode.iT;
 var leafTest = testmode.lT;
 var getTestMode = testmode.gT;

 T.assertParams = function (cl,pl,str) {
   var clreal = [];
   for (var i in cl) {
     clreal.push(T.getVar(cl[i]));
   };
   var ret = [];
   if (getTestMode()) {
     ret = clreal;
   } else {
     ret = [];
     for (var i in cl) {
       var c = T.getVar(cl[i]);
       if (c.checkParams(pl)) {
         ret.push(c);
       };
     };
     if (ret.length < 1) {
       var pla = [];
       for (var i = 0; i < pl.length; i++) pla.push(pl[i]);
       logAssertParam(clreal,pla,str);
     }
   };
   ret.assertReturn = function(v) {
     if (getTestMode()) return v;
     for (var i in ret) {
       c.checkReturn(v);
     };
     return v;
   };
   return ret;
 };
 T.overrideToStringOfFunction = function (f,fstr) {
   f.toString = function () { 
     return "" + fstr;
   };
 };

 return T;
})({});


