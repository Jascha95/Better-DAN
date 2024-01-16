var p = 
    (function () {
        function gen() { //TODO make this more specific, there are differnt kinds of Packets
            var kind = 0;//JSConTest.gen.genIInt(0,1);
            var id = 4;
            var packet = JSConTest.effects.newCall(Packet,[null,id,kind]);

            return packet;
        }
        function test(x) {
            return (x instanceof Packet);  
        }
        return new JSConTest.contracts.SContract(test,gen,"Packet");
     })();

var s =
  (function () {
        function gen() {
            return JSConTest.effects.newCall(Scheduler,[]);
        }
        function test(x) {
            return (x instanceof Scheduler);  
        }
        return new JSConTest.contracts.SContract(test,gen,"Scheduler");
     })();

var sinit =
  (function () {
        function gen() {
             var task = t.gen();
             return task.scheduler;
        }
        function test(x) {
            return (x instanceof Scheduler);  
        }
        return new JSConTest.contracts.SContract(test,gen,"SchedulerInitialized");
     })();

var ht =
    (function () {
        function gen() {
            var scheduler = s.gen();
            var priority = JSConTest.gen.genInt();
            var handlerTask = JSConTest.effects.newCall(HandlerTask,[scheduler]);
            scheduler.addTask(ID_HANDLER_A, priority, null, handlerTask);
            return handlerTask;
        }
        function test(x) {
            return (x instanceof HandlerTask);  
        }
        return new JSConTest.contracts.SContract(test,gen,"HandlerTask");
     })(); 

var wt =
    (function () {
        function gen() {
            var v1 = JSConTest.gen.genInt();
            var v2 = JSConTest.gen.genInt();
            var scheduler = s.gen();
            var priority = JSConTest.gen.genInt();
            var workerTask = JSConTest.effects.newCall(WorkerTask,[scheduler,v1,v2]);
            scheduler.addTask(ID_WORKER, priority, null, workerTask);
            return workerTask;
        }
        function test(x) {
            return (x instanceof WorkerTask);  
        }
        return new JSConTest.contracts.SContract(test,gen,"WorkerTask");
     })(); 

var it =
    (function () {
        function gen() {
            var v1 = JSConTest.gen.genInt();
            var count = JSConTest.gen.genInt();
            var scheduler = s.gen();
            var idleTask = JSConTest.effects.newCall(IdleTask,[scheduler,v1,count]);
            var priority = JSConTest.gen.genInt();
            scheduler.addRunningTask(ID_IDLE, priority, null, idleTask);
            return idleTask;
        }
        function test(x) {
            return (x instanceof IdleTask);  
        }
        return new JSConTest.contracts.SContract(test,gen,"IdleTask");
     })(); 

var dt =
    (function () {
        function gen() {
            var scheduler = s.gen();
            var priority = JSConTest.gen.genInt();
            var devTask = JSConTest.effects.newCall(DeviceTask,[scheduler]);
            scheduler.addTask(ID_DEVICE_A, priority, null, devTask);
            return devTask;
        }
        function test(x) {
            return (x instanceof DeviceTask);  
        }
        return new JSConTest.contracts.SContract(test,gen,"DeviceTask");
     })(); 

var tcb =
    (function () {
          var obj = { 
               getcdes : function() {return "";},
               arity : 1,
               f : function (link) {
                    var id = JSConTest.gen.genIInt(0,5);
                    var priority = JSConTest.gen.genInt();
                    var queue = p.gen();
                    var task = t.gen(); 
                    return JSConTest.effects.newCall(TaskControlBlock,[link, id, priority, queue, task]);
               }
          };
          function accept(v) {
               return (v !== null);
          }
          function genLeaf() {
             return obj.f(null);
          }

          function test(x) {
            return (x instanceof TaskControlBlock);  
          }
          function gen() {
            return JSConTest.gen.genTree(accept,[genLeaf],[obj],0.5,true);
          }

        return new JSConTest.contracts.SContract(test,gen,"TaskControlBlock");

     })(); 

var nulltcb = JSConTest.contracts.Union(tcb,JSConTest.contracts.SContract(JSConTest.check.isNull,JSConTest.gen.genNull,"Null"));
var undeftcb = JSConTest.contracts.Union(tcb,JSConTest.contracts.SContract(JSConTest.check.isUndefined,JSConTest.gen.genUndefined,"Undefined"));
var nullundeftcb = JSConTest.contracts.Union(nulltcb,undeftcb); 

var t = JSConTest.contracts.Union(JSConTest.contracts.Union(JSConTest.contracts.Union(dt,it),wt),ht);   
var nullt = JSConTest.contracts.Union(t,JSConTest.contracts.SContract(JSConTest.check.isNull,JSConTest.gen.genNull,"Null"));
var nullp = JSConTest.contracts.Union(p,JSConTest.contracts.SContract(JSConTest.check.isNull,JSConTest.gen.genNull,"Null"));


var tcbt = JSConTest.contracts.Union(t,tcb);


var log_output;
var log = (function () {
  var s = "";
  
  log_output = function () {
    return s;
  }
  function log(str) {
    s += str;
  }
  return log;
}());
