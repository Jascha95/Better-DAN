var runRichards = JSConTest.tests.addContracts("f_runRichards0", JSConTest.tests.setVar("f_runRichards0", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function runRichards () {
      var scheduler = JSConTest.effects.newCall(Scheduler, []);
        JSConTest.effects.mCall(scheduler, "addIdleTask", [ID_IDLE, 0. , null, COUNT]);
        var queue = JSConTest.effects.newCall(Packet, [null, ID_WORKER, KIND_WORK]);
        queue = JSConTest.effects.newCall(Packet, [queue, ID_WORKER, KIND_WORK]);
        JSConTest.effects.mCall(scheduler, "addWorkerTask", [ID_WORKER, 1000. , queue]);
        queue = JSConTest.effects.newCall(Packet, [null, ID_DEVICE_A, KIND_DEVICE]);
        queue = JSConTest.effects.newCall(Packet, [queue, ID_DEVICE_A, KIND_DEVICE]);
        queue = JSConTest.effects.newCall(Packet, [queue, ID_DEVICE_A, KIND_DEVICE]);
        JSConTest.effects.mCall(scheduler, "addHandlerTask", [ID_HANDLER_A, 2000. , queue]);
        queue = JSConTest.effects.newCall(Packet, [null, ID_DEVICE_B, KIND_DEVICE]);
        queue = JSConTest.effects.newCall(Packet, [queue, ID_DEVICE_B, KIND_DEVICE]);
        queue = JSConTest.effects.newCall(Packet, [queue, ID_DEVICE_B, KIND_DEVICE]);
        JSConTest.effects.mCall(scheduler, "addHandlerTask", [ID_HANDLER_B, 3000. , queue]);
        JSConTest.effects.mCall(scheduler, "addDeviceTask", [ID_DEVICE_A, 4000. , null]);
        JSConTest.effects.mCall(scheduler, "addDeviceTask", [ID_DEVICE_B, 5000. , null]);
        JSConTest.effects.mCall(scheduler, "schedule", []);
        JSConTest.effects.mCall(console, "log", [log_output()]);
        if (JSConTest.effects.unbox(((JSConTest.effects.unbox(JSConTest.effects.propAcc(scheduler, "queueCount")) != JSConTest.effects.unbox(EXPECTED_QUEUE_COUNT)) || (JSConTest.effects.unbox(JSConTest.effects.propAcc(scheduler, "holdCount")) != JSConTest.effects.unbox(EXPECTED_HOLD_COUNT))))) {
            var msg = (JSConTest.effects.unbox((JSConTest.effects.unbox((JSConTest.effects.unbox(("Error during execution: queueCount = " + JSConTest.effects.unbox(JSConTest.effects.propAcc(scheduler, "queueCount")))) + ", holdCount = ")) + JSConTest.effects.unbox(JSConTest.effects.propAcc(scheduler, "holdCount")))) + ".");
            throw JSConTest.effects.newCall(Error, [msg])
          };
    }), []), (function runRichards () {
      var scheduler = new Scheduler();
        scheduler.addIdleTask(ID_IDLE, 0. , null, COUNT);
        var queue = new Packet(null, ID_WORKER, KIND_WORK);
        queue = new Packet(queue, ID_WORKER, KIND_WORK);
        scheduler.addWorkerTask(ID_WORKER, 1000. , queue);
        queue = new Packet(null, ID_DEVICE_A, KIND_DEVICE);
        queue = new Packet(queue, ID_DEVICE_A, KIND_DEVICE);
        queue = new Packet(queue, ID_DEVICE_A, KIND_DEVICE);
        scheduler.addHandlerTask(ID_HANDLER_A, 2000. , queue);
        queue = new Packet(null, ID_DEVICE_B, KIND_DEVICE);
        queue = new Packet(queue, ID_DEVICE_B, KIND_DEVICE);
        queue = new Packet(queue, ID_DEVICE_B, KIND_DEVICE);
        scheduler.addHandlerTask(ID_HANDLER_B, 3000. , queue);
        scheduler.addDeviceTask(ID_DEVICE_A, 4000. , null);
        scheduler.addDeviceTask(ID_DEVICE_B, 5000. , null);
        scheduler.schedule();
        console.log(log_output());
        if (((scheduler.queueCount != EXPECTED_QUEUE_COUNT) || (scheduler.holdCount != EXPECTED_HOLD_COUNT))) {
            var msg = (((("Error during execution: queueCount = " + scheduler.queueCount) + ", holdCount = ") + scheduler.holdCount) + ".");
            throw new Error(msg)
          };
    }), false)), [], []);
  var Scheduler = JSConTest.tests.addContracts("f_Scheduler1", JSConTest.tests.setVar("f_Scheduler1", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function Scheduler () {
      JSConTest.effects.propAss(this, "queueCount", 0. );
        JSConTest.effects.propAss(this, "holdCount", 0. );
        JSConTest.effects.propAss(this, "blocks", JSConTest.effects.newCall(Array, [NUMBER_OF_IDS]));
        JSConTest.effects.propAss(this, "list", null);
        JSConTest.effects.propAss(this, "currentTcb", null);
        JSConTest.effects.propAss(this, "currentId", null);
    }), []), (function Scheduler () {
      this.queueCount = 0. ;
        this.holdCount = 0. ;
        this.blocks = new Array(NUMBER_OF_IDS);
        this.list = null;
        this.currentTcb = null;
        this.currentId = null;
    }), false)), [], []);
  var TaskControlBlock = JSConTest.tests.addContracts("f_TaskControlBlock24", JSConTest.tests.setVar("f_TaskControlBlock24", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function TaskControlBlock (link, idd, priority, queue, task) {
      JSConTest.effects.propAss(this, "link", link);
        JSConTest.effects.propAss(this, "idd", idd);
        JSConTest.effects.propAss(this, "priority", priority);
        JSConTest.effects.propAss(this, "queue", queue);
        JSConTest.effects.propAss(this, "task", task);
        if (JSConTest.effects.unbox((JSConTest.effects.unbox(queue) == null))) {
            JSConTest.effects.propAss(this, "state", STATE_SUSPENDED);
          }
          else {
            JSConTest.effects.propAss(this, "state", STATE_SUSPENDED_RUNNABLE);
          };
    }), ["link", "idd", "priority", "queue", "task"]), (function TaskControlBlock (link, idd, priority, queue, task) {
      this.link = link;
        this.idd = idd;
        this.priority = priority;
        this.queue = queue;
        this.task = task;
        if ((queue == null)) {
            this.state = STATE_SUSPENDED;
          }
          else {
            this.state = STATE_SUSPENDED_RUNNABLE;
          };
    }), false)), [], []);
  var IdleTask = JSConTest.tests.addContracts("f_IdleTask41", JSConTest.tests.setVar("f_IdleTask41", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function IdleTask (scheduler, v1, count) {
      JSConTest.effects.propAss(this, "scheduler", scheduler);
        JSConTest.effects.propAss(this, "v1", v1);
        JSConTest.effects.propAss(this, "count", count);
    }), ["scheduler", "v1", "count"]), (function IdleTask (scheduler, v1, count) {
      this.scheduler = scheduler;
        this.v1 = v1;
        this.count = count;
    }), false)), [], []);
  var DeviceTask = JSConTest.tests.addContracts("f_DeviceTask46", JSConTest.tests.setVar("f_DeviceTask46", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function DeviceTask (scheduler) {
      JSConTest.effects.propAss(this, "scheduler", scheduler);
        JSConTest.effects.propAss(this, "v1", null);
    }), ["scheduler"]), (function DeviceTask (scheduler) {
      this.scheduler = scheduler;
        this.v1 = null;
    }), false)), [], []);
  var WorkerTask = JSConTest.tests.addContracts("f_WorkerTask51", JSConTest.tests.setVar("f_WorkerTask51", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function WorkerTask (scheduler, v1, v2) {
      JSConTest.effects.propAss(this, "scheduler", scheduler);
        JSConTest.effects.propAss(this, "v1", v1);
        JSConTest.effects.propAss(this, "v2", v2);
    }), ["scheduler", "v1", "v2"]), (function WorkerTask (scheduler, v1, v2) {
      this.scheduler = scheduler;
        this.v1 = v1;
        this.v2 = v2;
    }), false)), [], []);
  var HandlerTask = JSConTest.tests.addContracts("f_HandlerTask56", JSConTest.tests.setVar("f_HandlerTask56", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function HandlerTask (scheduler) {
      JSConTest.effects.propAss(this, "scheduler", scheduler);
        JSConTest.effects.propAss(this, "v1", null);
        JSConTest.effects.propAss(this, "v2", null);
    }), ["scheduler"]), (function HandlerTask (scheduler) {
      this.scheduler = scheduler;
        this.v1 = null;
        this.v2 = null;
    }), false)), [], []);
  var Packet = JSConTest.tests.addContracts("f_Packet61", JSConTest.tests.setVar("f_Packet61", JSConTest.tests.overrideToStringOfFunction(JSConTest.effects.enableWrapper((function Packet (link, idd, kind) {
      JSConTest.effects.propAss(this, "link", link);
        JSConTest.effects.propAss(this, "idd", idd);
        JSConTest.effects.propAss(this, "kind", kind);
        JSConTest.effects.propAss(this, "a1", 0. );
        JSConTest.effects.propAss(this, "a2", JSConTest.effects.newCall(Array, [DATA_SIZE]));
    }), ["link", "idd", "kind"]), (function Packet (link, idd, kind) {
      this.link = link;
        this.idd = idd;
        this.kind = kind;
        this.a1 = 0. ;
        this.a2 = new Array(DATA_SIZE);
    }), false)), [], []);
  var Richards = new BenchmarkSuite("Richards", 34886. , [new Benchmark("Richards", runRichards)]);
  var COUNT = 1000. ;
  var EXPECTED_QUEUE_COUNT = 2322. ;
  var EXPECTED_HOLD_COUNT = 928. ;
  var ID_IDLE = 0. ;
  var ID_WORKER = 1. ;
  var ID_HANDLER_A = 2. ;
  var ID_HANDLER_B = 3. ;
  var ID_DEVICE_A = 4. ;
  var ID_DEVICE_B = 5. ;
  var NUMBER_OF_IDS = 6. ;
  var KIND_DEVICE = 0. ;
  var KIND_WORK = 1. ;
  Scheduler.prototype.addIdleTask = JSConTest.tests.addContracts("f_addIdleTask2", JSConTest.tests.setVar("f_addIdleTask2", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function addIdleTask (idd, priority, queue, count) {
    JSConTest.effects.mCall(this, "addRunningTask", [idd, priority, queue, new IdleTask(this, 1. , count)]);
  }), ["idd", "priority", "queue", "count"]), ["c_3"], "f_addIdleTask2"), (function addIdleTask (idd, priority, queue, count) {
    this.addRunningTask(idd, priority, queue, new IdleTask(this, 1. , count));
  }), true)), [{contract : JSConTest.tests.setVar("c_3", JSConTest.contracts.Method(s, [JSConTest.contracts.IIntervall(0. , 5. ), JSConTest.contracts.Integer, nullp, JSConTest.contracts.Integer], JSConTest.contracts.Undefined, [], "f_addIdleTask2")), count : 1000. }], []);
  Scheduler.prototype.addWorkerTask = JSConTest.tests.addContracts("f_addWorkerTask4", JSConTest.tests.setVar("f_addWorkerTask4", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function addWorkerTask (idd, priority, queue) {
    JSConTest.effects.mCall(this, "addTask", [idd, priority, queue, new WorkerTask(this, ID_HANDLER_A, 0. )]);
  }), ["idd", "priority", "queue"]), ["c_5"], "f_addWorkerTask4"), (function addWorkerTask (idd, priority, queue) {
    this.addTask(idd, priority, queue, new WorkerTask(this, ID_HANDLER_A, 0. ));
  }), true)), [{contract : JSConTest.tests.setVar("c_5", JSConTest.contracts.Method(s, [JSConTest.contracts.IIntervall(0. , 5. ), JSConTest.contracts.Integer, nullp], JSConTest.contracts.Undefined, [], "f_addWorkerTask4")), count : 1000. }], []);
  Scheduler.prototype.addHandlerTask = JSConTest.tests.addContracts("f_addHandlerTask6", JSConTest.tests.setVar("f_addHandlerTask6", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function addHandlerTask (idd, priority, queue) {
    JSConTest.effects.mCall(this, "addTask", [idd, priority, queue, new HandlerTask(this)]);
  }), ["idd", "priority", "queue"]), ["c_7"], "f_addHandlerTask6"), (function addHandlerTask (idd, priority, queue) {
    this.addTask(idd, priority, queue, new HandlerTask(this));
  }), true)), [{contract : JSConTest.tests.setVar("c_7", JSConTest.contracts.Method(s, [JSConTest.contracts.IIntervall(0. , 5. ), JSConTest.contracts.Integer, nullp], JSConTest.contracts.Undefined, [], "f_addHandlerTask6")), count : 1000. }], []);
  Scheduler.prototype.addDeviceTask = JSConTest.tests.addContracts("f_addDeviceTask8", JSConTest.tests.setVar("f_addDeviceTask8", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function addDeviceTask (idd, priority, queue) {
    JSConTest.effects.mCall(this, "addTask", [idd, priority, queue, new DeviceTask(this)]);
  }), ["idd", "priority", "queue"]), ["c_9"], "f_addDeviceTask8"), (function addDeviceTask (idd, priority, queue) {
    this.addTask(idd, priority, queue, new DeviceTask(this));
  }), true)), [{contract : JSConTest.tests.setVar("c_9", JSConTest.contracts.Method(s, [JSConTest.contracts.IIntervall(0. , 5. ), JSConTest.contracts.Integer, nullp], JSConTest.contracts.Undefined, [], "f_addDeviceTask8")), count : 1000. }], []);
  Scheduler.prototype.addRunningTask = JSConTest.tests.addContracts("f_addRunningTask10", JSConTest.tests.setVar("f_addRunningTask10", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function addRunningTask (idd, priority, queue, task) {
    JSConTest.effects.mCall(this, "addTask", [idd, priority, queue, task]);
      JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "currentTcb"), "setRunning", []);
  }), ["idd", "priority", "queue", "task"]), ["c_11"], "f_addRunningTask10"), (function addRunningTask (idd, priority, queue, task) {
    this.addTask(idd, priority, queue, task);
      this.currentTcb.setRunning();
  }), true)), [{contract : JSConTest.tests.setVar("c_11", JSConTest.contracts.Method(s, [JSConTest.contracts.IIntervall(0. , 5. ), JSConTest.contracts.Integer, nullp, it], JSConTest.contracts.Undefined, [], "f_addRunningTask10")), count : 1000. }], []);
  Scheduler.prototype.addTask = JSConTest.tests.addContracts("f_addTask12", JSConTest.tests.setVar("f_addTask12", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function addTask (idd, priority, queue, task) {
    JSConTest.effects.propAss(this, "currentTcb", JSConTest.effects.newCall(TaskControlBlock, [JSConTest.effects.propAcc(this, "list"), idd, priority, queue, task]));
      JSConTest.effects.propAss(this, "list", JSConTest.effects.propAcc(this, "currentTcb"));
      JSConTest.effects.propAss(JSConTest.effects.propAcc(this, "blocks"), idd, JSConTest.effects.propAcc(this, "currentTcb"));
  }), ["idd", "priority", "queue", "task"]), ["c_13"], "f_addTask12"), (function addTask (idd, priority, queue, task) {
    this.currentTcb = new TaskControlBlock(this.list, idd, priority, queue, task);
      this.list = this.currentTcb;
      this.blocks[idd] = this.currentTcb;
  }), true)), [{contract : JSConTest.tests.setVar("c_13", JSConTest.contracts.Method(s, [JSConTest.contracts.IIntervall(0. , 5. ), JSConTest.contracts.Integer, nullp, t], JSConTest.contracts.Undefined, [], "f_addTask12")), count : 1000. }], []);
  Scheduler.prototype.schedule = JSConTest.tests.addContracts("f_schedule14", JSConTest.tests.setVar("f_schedule14", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function schedule () {
    JSConTest.effects.propAss(this, "currentTcb", JSConTest.effects.propAcc(this, "list"));
      while (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "currentTcb")) != null))) {
          if (JSConTest.effects.unbox(JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "currentTcb"), "isHeldOrSuspended", []))) {
            JSConTest.effects.propAss(this, "currentTcb", JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "currentTcb"), "link"));
          }
          else {
            JSConTest.effects.propAss(this, "currentId", JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "currentTcb"), "idd"));
            JSConTest.effects.fCall(log, [(this.currentId + " ")]);
            JSConTest.effects.propAss(this, "currentTcb", JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "currentTcb"), "run", []));
          };
        }
  }), []), ["c_15"], "f_schedule14"), (function schedule () {
    this.currentTcb = this.list;
      while ((this.currentTcb != null)) {
          if (this.currentTcb.isHeldOrSuspended()) {
            this.currentTcb = this.currentTcb.link;
          }
          else {
            this.currentId = this.currentTcb.idd;
            log((this.currentId + " "));
            this.currentTcb = this.currentTcb.run();
          };
        }
  }), true)), [{contract : JSConTest.tests.setVar("c_15", JSConTest.contracts.Method(s, [], JSConTest.contracts.Undefined, [], "f_schedule14")), count : 1000. }], []);
  Scheduler.prototype.release = JSConTest.tests.addContracts("f_release16", JSConTest.tests.setVar("f_release16", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function release (idd) {
    var tcb = JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "blocks"), idd);
      if (JSConTest.effects.unbox((JSConTest.effects.unbox(tcb) == null))) {
          return tcb;
        };
      JSConTest.effects.mCall(tcb, "markAsNotHeld", []);
      if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(tcb, "priority")) > JSConTest.effects.unbox(JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "currentTcb"), "priority"))))) {
          return tcb;
        }
        else {
          return JSConTest.effects.propAcc(this, "currentTcb");
        };
  }), ["idd"]), ["c_17"], "f_release16"), (function release (idd) {
    var tcb = this.blocks[idd];
      if ((tcb == null)) {
          return tcb;
        };
      tcb.markAsNotHeld();
      if ((tcb.priority > this.currentTcb.priority)) {
          return tcb;
        }
        else {
          return this.currentTcb;
        };
  }), true)), [{contract : JSConTest.tests.setVar("c_17", JSConTest.contracts.Method(s, [JSConTest.contracts.IIntervall(0. , 5. )], undeftcb, [], "f_release16")), count : 1000. }], []);
  Scheduler.prototype.holdCurrent = JSConTest.tests.addContracts("f_holdCurrent18", JSConTest.tests.setVar("f_holdCurrent18", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function holdCurrent () {
    JSConTest.effects.unOp(1. , this, "holdCount");
      JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "currentTcb"), "markAsHeld", []);
      return JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "currentTcb"), "link");
  }), []), ["c_19"], "f_holdCurrent18"), (function holdCurrent () {
    this.holdCount++ ;
      this.currentTcb.markAsHeld();
      return this.currentTcb.link;
  }), true)), [{contract : JSConTest.tests.setVar("c_19", JSConTest.contracts.Method(sinit, [], nulltcb, [], "f_holdCurrent18")), count : 1000. }], []);
  Scheduler.prototype.suspendCurrent = JSConTest.tests.addContracts("f_suspendCurrent20", JSConTest.tests.setVar("f_suspendCurrent20", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function suspendCurrent () {
    JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "currentTcb"), "markAsSuspended", []);
      return JSConTest.effects.propAcc(this, "currentTcb");
  }), []), ["c_21"], "f_suspendCurrent20"), (function suspendCurrent () {
    this.currentTcb.markAsSuspended();
      return this.currentTcb;
  }), true)), [{contract : JSConTest.tests.setVar("c_21", JSConTest.contracts.Method(sinit, [], nulltcb, [], "f_suspendCurrent20")), count : 1000. }], []);
  Scheduler.prototype.queue = JSConTest.tests.addContracts("f_queue22", JSConTest.tests.setVar("f_queue22", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function queue (packet) {
    var t = JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "blocks"), JSConTest.effects.propAcc(packet, "idd"));
      if (JSConTest.effects.unbox((JSConTest.effects.unbox(t) == null))) return t;;
      JSConTest.effects.unOp(1. , this, "queueCount");
      JSConTest.effects.propAss(packet, "link", null);
      JSConTest.effects.propAss(packet, "idd", JSConTest.effects.propAcc(this, "currentId"));
      return JSConTest.effects.mCall(t, "checkPriorityAdd", [this.currentTcb, packet]);
  }), ["packet"]), ["c_23"], "f_queue22"), (function queue (packet) {
    var t = this.blocks[packet.idd];
      if ((t == null)) return t;;
      this.queueCount++ ;
      packet.link = null;
      packet.idd = this.currentId;
      return t.checkPriorityAdd(this.currentTcb, packet);
  }), true)), [{contract : JSConTest.tests.setVar("c_23", JSConTest.contracts.Method(s, [p], undeftcb, [], "f_queue22")), count : 1000. }], []);
  var STATE_RUNNING = 0. ;
  var STATE_RUNNABLE = 1. ;
  var STATE_SUSPENDED = 2. ;
  var STATE_HELD = 4. ;
  var STATE_SUSPENDED_RUNNABLE = (STATE_SUSPENDED | STATE_RUNNABLE);
  var STATE_NOT_HELD =  ~STATE_HELD;
  TaskControlBlock.prototype.setRunning = JSConTest.tests.addContracts("f_setRunning25", JSConTest.tests.setVar("f_setRunning25", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function setRunning () {
    JSConTest.effects.propAss(this, "state", STATE_RUNNING);
  }), []), ["c_26"], "f_setRunning25"), (function setRunning () {
    this.state = STATE_RUNNING;
  }), true)), [{contract : JSConTest.tests.setVar("c_26", JSConTest.contracts.Method(tcb, [], JSConTest.contracts.Undefined, [], "f_setRunning25")), count : 1000. }], []);
  TaskControlBlock.prototype.markAsNotHeld = JSConTest.tests.addContracts("f_markAsNotHeld27", JSConTest.tests.setVar("f_markAsNotHeld27", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function markAsNotHeld () {
    JSConTest.effects.propAss(this, "state", (JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "state")) & JSConTest.effects.unbox(STATE_NOT_HELD)));
  }), []), ["c_28"], "f_markAsNotHeld27"), (function markAsNotHeld () {
    this.state = (this.state & STATE_NOT_HELD);
  }), true)), [{contract : JSConTest.tests.setVar("c_28", JSConTest.contracts.Method(tcb, [], JSConTest.contracts.Undefined, [], "f_markAsNotHeld27")), count : 1000. }], []);
  TaskControlBlock.prototype.markAsHeld = JSConTest.tests.addContracts("f_markAsHeld29", JSConTest.tests.setVar("f_markAsHeld29", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function markAsHeld () {
    JSConTest.effects.propAss(this, "state", (JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "state")) | JSConTest.effects.unbox(STATE_HELD)));
  }), []), ["c_30"], "f_markAsHeld29"), (function markAsHeld () {
    this.state = (this.state | STATE_HELD);
  }), true)), [{contract : JSConTest.tests.setVar("c_30", JSConTest.contracts.Method(tcb, [], JSConTest.contracts.Undefined, [], "f_markAsHeld29")), count : 1000. }], []);
  TaskControlBlock.prototype.isHeldOrSuspended = JSConTest.tests.addContracts("f_isHeldOrSuspended31", JSConTest.tests.setVar("f_isHeldOrSuspended31", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function isHeldOrSuspended () {
    return ((JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "state")) & JSConTest.effects.unbox(STATE_HELD))) != 0. ) || (JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "state")) == JSConTest.effects.unbox(STATE_SUSPENDED)));
  }), []), ["c_32"], "f_isHeldOrSuspended31"), (function isHeldOrSuspended () {
    return (((this.state & STATE_HELD) != 0. ) || (this.state == STATE_SUSPENDED));
  }), true)), [{contract : JSConTest.tests.setVar("c_32", JSConTest.contracts.Method(tcb, [], JSConTest.contracts.Boolean, [], "f_isHeldOrSuspended31")), count : 1000. }], []);
  TaskControlBlock.prototype.markAsSuspended = JSConTest.tests.addContracts("f_markAsSuspended33", JSConTest.tests.setVar("f_markAsSuspended33", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function markAsSuspended () {
    JSConTest.effects.propAss(this, "state", (JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "state")) | JSConTest.effects.unbox(STATE_SUSPENDED)));
  }), []), ["c_34"], "f_markAsSuspended33"), (function markAsSuspended () {
    this.state = (this.state | STATE_SUSPENDED);
  }), true)), [{contract : JSConTest.tests.setVar("c_34", JSConTest.contracts.Method(tcb, [], JSConTest.contracts.Undefined, [], "f_markAsSuspended33")), count : 1000. }], []);
  TaskControlBlock.prototype.markAsRunnable = JSConTest.tests.addContracts("f_markAsRunnable35", JSConTest.tests.setVar("f_markAsRunnable35", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function markAsRunnable () {
    JSConTest.effects.propAss(this, "state", (JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "state")) | JSConTest.effects.unbox(STATE_RUNNABLE)));
  }), []), ["c_36"], "f_markAsRunnable35"), (function markAsRunnable () {
    this.state = (this.state | STATE_RUNNABLE);
  }), true)), [{contract : JSConTest.tests.setVar("c_36", JSConTest.contracts.Method(tcb, [], JSConTest.contracts.Undefined, [], "f_markAsRunnable35")), count : 1000. }], []);
  TaskControlBlock.prototype.run = JSConTest.tests.addContracts("f_run_tcb37", JSConTest.tests.setVar("f_run_tcb37", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function run_tcb () {
    var packet;
      if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "state")) == JSConTest.effects.unbox(STATE_SUSPENDED_RUNNABLE)))) {
          packet = JSConTest.effects.propAcc(this, "queue");
          JSConTest.effects.propAss(this, "queue", JSConTest.effects.propAcc(packet, "link"));
          if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "queue")) == null))) {
            JSConTest.effects.propAss(this, "state", STATE_RUNNING);
          }
          else {
            JSConTest.effects.propAss(this, "state", STATE_RUNNABLE);
          };
        }
        else {
          packet = null;
        };
      return JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "task"), "run", [packet]);
  }), []), ["c_38"], "f_run_tcb37"), (function run_tcb () {
    var packet;
      if ((this.state == STATE_SUSPENDED_RUNNABLE)) {
          packet = this.queue;
          this.queue = packet.link;
          if ((this.queue == null)) {
            this.state = STATE_RUNNING;
          }
          else {
            this.state = STATE_RUNNABLE;
          };
        }
        else {
          packet = null;
        };
      return this.task.run(packet);
  }), true)), [{contract : JSConTest.tests.setVar("c_38", JSConTest.contracts.Method(tcb, [], nullundeftcb, [], "f_run_tcb37")), count : 1000. }], []);
  TaskControlBlock.prototype.checkPriorityAdd = JSConTest.tests.addContracts("f_checkPriorityAdd39", JSConTest.tests.setVar("f_checkPriorityAdd39", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function checkPriorityAdd (task, packet) {
    if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "queue")) == null))) {
          JSConTest.effects.propAss(this, "queue", packet);
          JSConTest.effects.mCall(this, "markAsRunnable", []);
          if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "priority")) > JSConTest.effects.unbox(JSConTest.effects.propAcc(task, "priority"))))) return this;;
        }
        else {
          JSConTest.effects.propAss(this, "queue", JSConTest.effects.mCall(packet, "addTo", [this.queue]));
        };
      return task;
  }), ["task", "packet"]), ["c_40"], "f_checkPriorityAdd39"), (function checkPriorityAdd (task, packet) {
    if ((this.queue == null)) {
          this.queue = packet;
          this.markAsRunnable();
          if ((this.priority > task.priority)) return this;;
        }
        else {
          this.queue = packet.addTo(this.queue);
        };
      return task;
  }), true)), [{contract : JSConTest.tests.setVar("c_40", JSConTest.contracts.Method(tcb, [tcb, p], tcb, [], "f_checkPriorityAdd39")), count : 1000. }], []);
  TaskControlBlock.prototype.toString = (function toString () {
    return (((("tcb { " + this.task) + "@") + this.state) + " }");
  });
  IdleTask.prototype.run = JSConTest.tests.addContracts("f_run_it42", JSConTest.tests.setVar("f_run_it42", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function run_it (packet) {
    JSConTest.effects.unOp(2. , this, "count");
      if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "count")) == 0. ))) return JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "scheduler"), "holdCurrent", []);;
      if (JSConTest.effects.unbox((JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "v1")) & 1. )) == 0. ))) {
          JSConTest.effects.propAss(this, "v1", (JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "v1")) >> 1. ));
          return JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "scheduler"), "release", [ID_DEVICE_A]);
        }
        else {
          JSConTest.effects.propAss(this, "v1", (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "v1")) >> 1. )) ^ 53256. ));
          return JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "scheduler"), "release", [ID_DEVICE_B]);
        };
  }), ["packet"]), ["c_43"], "f_run_it42"), (function run_it (packet) {
    this.count-- ;
      if ((this.count == 0. )) return this.scheduler.holdCurrent();;
      if (((this.v1 & 1. ) == 0. )) {
          this.v1 = (this.v1 >> 1. );
          return this.scheduler.release(ID_DEVICE_A);
        }
        else {
          this.v1 = ((this.v1 >> 1. ) ^ 53256. );
          return this.scheduler.release(ID_DEVICE_B);
        };
  }), true)), [{contract : JSConTest.tests.setVar("c_43", JSConTest.contracts.Method(it, [p], undeftcb, [], "f_run_it42")), count : 1000. }], []);
  IdleTask.prototype.toString = JSConTest.tests.addContracts("f_toString_it44", JSConTest.tests.setVar("f_toString_it44", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function toString_it () {
    return "IdleTask";
  }), []), ["c_45"], "f_toString_it44"), (function toString_it () {
    return "IdleTask";
  }), true)), [{contract : JSConTest.tests.setVar("c_45", JSConTest.contracts.Method(it, [], JSConTest.contracts.String, [], "f_toString_it44")), count : 1000. }], []);
  DeviceTask.prototype.run = JSConTest.tests.addContracts("f_run_dt47", JSConTest.tests.setVar("f_run_dt47", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function run_dt (packet) {
    if (JSConTest.effects.unbox((JSConTest.effects.unbox(packet) == null))) {
          if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "v1")) == null))) return JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "scheduler"), "suspendCurrent", []);;
          var v = JSConTest.effects.propAcc(this, "v1");
          JSConTest.effects.propAss(this, "v1", null);
          return JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "scheduler"), "queue", [v]);
        }
        else {
          JSConTest.effects.propAss(this, "v1", packet);
          return JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "scheduler"), "holdCurrent", []);
        };
  }), ["packet"]), ["c_48"], "f_run_dt47"), (function run_dt (packet) {
    if ((packet == null)) {
          if ((this.v1 == null)) return this.scheduler.suspendCurrent();;
          var v = this.v1;
          this.v1 = null;
          return this.scheduler.queue(v);
        }
        else {
          this.v1 = packet;
          return this.scheduler.holdCurrent();
        };
  }), true)), [{contract : JSConTest.tests.setVar("c_48", JSConTest.contracts.Method(dt, [p], nulltcb, [], "f_run_dt47")), count : 1000. }], []);
  DeviceTask.prototype.toString = JSConTest.tests.addContracts("f_toString_dt49", JSConTest.tests.setVar("f_toString_dt49", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function toString_dt () {
    return "DeviceTask";
  }), []), ["c_50"], "f_toString_dt49"), (function toString_dt () {
    return "DeviceTask";
  }), true)), [{contract : JSConTest.tests.setVar("c_50", JSConTest.contracts.Method(dt, [], JSConTest.contracts.String, [], "f_toString_dt49")), count : 1000. }], []);
  WorkerTask.prototype.run = JSConTest.tests.addContracts("f_run_wt52", JSConTest.tests.setVar("f_run_wt52", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function run_wt (packet) {
    if (JSConTest.effects.unbox((JSConTest.effects.unbox(packet) == null))) {
          return JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "scheduler"), "suspendCurrent", []);
        }
        else {
          if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "v1")) == JSConTest.effects.unbox(ID_HANDLER_A)))) {
            JSConTest.effects.propAss(this, "v1", ID_HANDLER_B);
          }
          else {
            JSConTest.effects.propAss(this, "v1", ID_HANDLER_A);
          };
          JSConTest.effects.propAss(packet, "idd", JSConTest.effects.propAcc(this, "v1"));
          JSConTest.effects.propAss(packet, "a1", 0. );
          for (var i = 0. ; JSConTest.effects.unbox((JSConTest.effects.unbox(i) < JSConTest.effects.unbox(DATA_SIZE))); i++ )  {
            JSConTest.effects.unOp(1. , this, "v2");
            if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "v2")) > 26. ))) JSConTest.effects.propAss(this, "v2", 1. );;
            JSConTest.effects.propAss(JSConTest.effects.propAcc(packet, "a2"), i, JSConTest.effects.propAcc(this, "v2"));
          };
          return JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "scheduler"), "queue", [packet]);
        };
  }), ["packet"]), ["c_53"], "f_run_wt52"), (function run_wt (packet) {
    if ((packet == null)) {
          return this.scheduler.suspendCurrent();
        }
        else {
          if ((this.v1 == ID_HANDLER_A)) {
            this.v1 = ID_HANDLER_B;
          }
          else {
            this.v1 = ID_HANDLER_A;
          };
          packet.idd = this.v1;
          packet.a1 = 0. ;
          for (var i = 0. ; (i < DATA_SIZE); i++ )  {
            this.v2++ ;
            if ((this.v2 > 26. )) this.v2 = 1. ;;
            packet.a2[i] = this.v2;
          };
          return this.scheduler.queue(packet);
        };
  }), true)), [{contract : JSConTest.tests.setVar("c_53", JSConTest.contracts.Method(wt, [nullp], undeftcb, [], "f_run_wt52")), count : 1000. }], []);
  WorkerTask.prototype.toString = JSConTest.tests.addContracts("f_toString_wt54", JSConTest.tests.setVar("f_toString_wt54", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function toString_wt () {
    return "WorkerTask";
  }), []), ["c_55"], "f_toString_wt54"), (function toString_wt () {
    return "WorkerTask";
  }), true)), [{contract : JSConTest.tests.setVar("c_55", JSConTest.contracts.Method(wt, [], JSConTest.contracts.String, [], "f_toString_wt54")), count : 1000. }], []);
  HandlerTask.prototype.run = JSConTest.tests.addContracts("f_run_ht57", JSConTest.tests.setVar("f_run_ht57", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function run_ht (packet) {
    if (JSConTest.effects.unbox((JSConTest.effects.unbox(packet) != null))) {
          if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(packet, "kind")) == JSConTest.effects.unbox(KIND_WORK)))) {
            JSConTest.effects.propAss(this, "v1", JSConTest.effects.mCall(packet, "addTo", [this.v1]));
          }
          else {
            JSConTest.effects.propAss(this, "v2", JSConTest.effects.mCall(packet, "addTo", [this.v2]));
          };
        };
      if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "v1")) != null))) {
          var count = JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "v1"), "a1");
          var v;
          if (JSConTest.effects.unbox((JSConTest.effects.unbox(count) < JSConTest.effects.unbox(DATA_SIZE)))) {
            if (JSConTest.effects.unbox((JSConTest.effects.unbox(JSConTest.effects.propAcc(this, "v2")) != null))) {
              v = JSConTest.effects.propAcc(this, "v2");
              JSConTest.effects.propAss(this, "v2", JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "v2"), "link"));
              JSConTest.effects.propAss(v, "a1", JSConTest.effects.propAcc(JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "v1"), "a2"), count));
              JSConTest.effects.propAss(JSConTest.effects.propAcc(this, "v1"), "a1", (JSConTest.effects.unbox(count) + 1. ));
              return JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "scheduler"), "queue", [v]);
            };
          }
          else {
            v = JSConTest.effects.propAcc(this, "v1");
            JSConTest.effects.propAss(this, "v1", JSConTest.effects.propAcc(JSConTest.effects.propAcc(this, "v1"), "link"));
            return JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "scheduler"), "queue", [v]);
          };
        };
      return JSConTest.effects.mCall(JSConTest.effects.propAcc(this, "scheduler"), "suspendCurrent", []);
  }), ["packet"]), ["c_58"], "f_run_ht57"), (function run_ht (packet) {
    if ((packet != null)) {
          if ((packet.kind == KIND_WORK)) {
            this.v1 = packet.addTo(this.v1);
          }
          else {
            this.v2 = packet.addTo(this.v2);
          };
        };
      if ((this.v1 != null)) {
          var count = this.v1.a1;
          var v;
          if ((count < DATA_SIZE)) {
            if ((this.v2 != null)) {
              v = this.v2;
              this.v2 = this.v2.link;
              v.a1 = this.v1.a2[count];
              this.v1.a1 = (count + 1. );
              return this.scheduler.queue(v);
            };
          }
          else {
            v = this.v1;
            this.v1 = this.v1.link;
            return this.scheduler.queue(v);
          };
        };
      return this.scheduler.suspendCurrent();
  }), true)), [{contract : JSConTest.tests.setVar("c_58", JSConTest.contracts.Method(ht, [p], nulltcb, [], "f_run_ht57")), count : 1000. }], []);
  HandlerTask.prototype.toString = JSConTest.tests.addContracts("f_toString_ht59", JSConTest.tests.setVar("f_toString_ht59", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function toString_ht () {
    return "HandlerTask";
  }), []), ["c_60"], "f_toString_ht59"), (function toString_ht () {
    return "HandlerTask";
  }), true)), [{contract : JSConTest.tests.setVar("c_60", JSConTest.contracts.Method(ht, [], JSConTest.contracts.String, [], "f_toString_ht59")), count : 1000. }], []);
  var DATA_SIZE = 4. ;
  Packet.prototype.addTo = JSConTest.tests.addContracts("f_addTo62", JSConTest.tests.setVar("f_addTo62", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function addTo (queue) {
    JSConTest.effects.propAss(this, "link", null);
      if (JSConTest.effects.unbox((JSConTest.effects.unbox(queue) == null))) return this;;
      var peek,
          next = queue;
      while (JSConTest.effects.unbox((JSConTest.effects.unbox(peek = JSConTest.effects.propAcc(next, "link")) != null))) next = peek;
      JSConTest.effects.propAss(next, "link", this);
      return queue;
  }), ["queue"]), ["c_63"], "f_addTo62"), (function addTo (queue) {
    this.link = null;
      if ((queue == null)) return this;;
      var peek,
          next = queue;
      while ((peek = next.link != null)) next = peek;
      next.link = this;
      return queue;
  }), true)), [{contract : JSConTest.tests.setVar("c_63", JSConTest.contracts.Method(p, [nullp], p, [], "f_addTo62")), count : 1000. }], []);
  Packet.prototype.toString = JSConTest.tests.addContracts("f_toString_p64", JSConTest.tests.setVar("f_toString_p64", JSConTest.tests.overrideToStringOfFunction(JSConTest.tests.enableAsserts(JSConTest.effects.enableWrapper((function toString_p () {
    return "Packet";
  }), []), ["c_65"], "f_toString_p64"), (function toString_p () {
    return "Packet";
  }), true)), [{contract : JSConTest.tests.setVar("c_65", JSConTest.contracts.Method(p, [], JSConTest.contracts.String, [], "f_toString_p64")), count : 1000. }], []);