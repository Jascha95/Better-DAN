(*

JIR Interpreter
===============

This is a simple and inefficient interpreter for JIR.

USAGE:
------

  (1) Write a module Jir matching the JIR signature.
  (2) Instantiate the Jir_Gen_Eval functor with this module to get
      a specialized evaluation module Jir_Eval; that is
      module Jir_Eval = Jir_Gen_Eval(Jir)
  (3) Call the run function of the Jir_Eval module. It has the following
      signature:

        val run : Jir.mach -> Jir.stmt array -> Jir.pc -> unit
  
      The first argument is the maching state of your Jir module, the
      second argument is the code (i.e., an array of statements), and
      the third argument is the index of the first statement to be
      executed.

EXAMPLE:
--------

Sample code is found at the end of this module.

CHANGES to the IR:
------------------

  * Introduced Exit syscall (the syscall doesn't take any arguments and doesn't
    return anything useful)

Author: Stefan Wehr (wehr@informatik.uni-freiburg.de)
Date: January 2007
License: GPL
*)

(* To use the interpreter, you have to write a module matching the
   following signature: *)
module type JIR =
sig
  (* abstract types for registers, addresses, and labels *)
  type reg
  type addr
  type label

  (* concrete types representing instructions *)
  type op   = Add | Sub | Mult | Div 
              | And | Or | Lt | Leq | Gt | Geq | Eq | Neq
  type uop  = UMinus | Not
  type sys  = Write | Read | Alloc | Exit
  type exp  = Const of int
              | Addr of addr 
              | Reg of reg
              | Get of exp
              | Binop of exp * op * exp
              | Unop of uop * exp
  type stmt = MoveMem of exp * exp
              | MoveReg of reg * exp
              | Call of reg * int * exp list
              | Syscall of reg * sys * exp list
              | Jump of exp * label
              | Label of label
              | Return of exp

  (* type synonyms *)
  type pc = int   (* the program counter *)
  type offset = int

  (* values *)
  type value = VInt of int | VAddr of addr
  val string_of_value : value -> string

  (* memory and register access *)
  type mach  (* abstract type representing memory and registers *) 
  val lookup_mem : mach -> addr -> value 
  val lookup_reg : mach -> reg -> value
  val set_mem : mach -> addr -> value -> mach
  val set_reg : mach -> reg -> value -> mach

  (* lookup functions for methods, and strings *)
  val lookup_method : mach -> addr -> offset -> (label * reg list)
  val lookup_string : mach -> addr -> string

  (* memory allocation *)
  val alloc_string : mach -> string -> (addr * mach)
  val alloc_mem : mach -> int -> (addr * mach)

  (* address manipulation *)
  val addr_offset : addr -> int -> addr
  val addr_equal : addr -> addr -> bool

  (* dumping and restoring registers *)
  type reg_dump
  val dump_registers : mach -> reg_dump
  val restore_registers : mach -> reg_dump -> mach
end

let string_of_list f l =
  let l' = List.map f l in
    "[" ^ String.concat ";" l' ^ "]"

let __debug = false

(* the signature of the JirEval functor *)
module type JIR_EVAL = 
  functor (Jir: JIR) ->
  sig
    val run : Jir.mach -> Jir.stmt array -> Jir.pc -> unit
  end

module Jir_Gen_Eval : JIR_EVAL = functor (Jir: JIR) ->
struct

  open Jir

  let string_of_op = function
    | Add -> "Add"
    | Sub -> "Sub"
    | Mult -> "Mult"
    | Div -> "Div"
    | And -> "And"
    | Or -> "Or"
    | Lt -> "Lt"
    | Leq -> "Leq"
    | Gt -> "Gt"
    | Geq -> "Geq"
    | Eq -> "Eq"
    | Neq -> "Neq"

  let string_of_unop = function
    | UMinus -> "UMinus"
    | Not -> "Not"

  let string_of_syscall = function
    | Alloc -> "Alloc"
    | Write -> "Write"
    | Read -> "Read"
    | Exit -> "Exit"

  exception Stop
  exception Jir_error of string
  let jir_error s = raise (Jir_error s)

  let debug s = 
    if __debug then print_endline ("[DEBUG] " ^ s) else ()

  (* tuple of current pc and stack of 
     return registers / pc / reg_dump *)
  type ctrl = (pc * (reg * pc * reg_dump) list)

  let init_ctrl pc = (pc, [])

  let inc_pc (pc, stack) = (pc + 1, stack)

  let enter_proc mach (pc, stack) result_reg target_pc =
    (target_pc, (result_reg, pc+1, dump_registers mach) :: stack)

  let set_pc (pc, stack) new_pc = (new_pc, stack)

  let exit_proc mach (pc, stack) =
    match stack with
      | [] -> jir_error ("cannot call return outside of any function")
      | ((return_reg, return_pc, reg_dump) :: stack') ->
          (return_reg, restore_registers mach reg_dump, (return_pc, stack'))

  let rec eval_exp mach = function
    | Const(i) -> VInt i
    | Addr(a) -> VAddr a
    | Reg(r) -> lookup_reg mach r
    | Get(e) ->
        begin
          match eval_exp mach e with
            | VAddr(a) -> lookup_mem mach a
            | x -> jir_error ("not an address: " ^ string_of_value x)
        end
    | Binop(e1, op, e2) ->
        begin
          match (op, eval_exp mach e1, eval_exp mach e2) with
            | (Add, VInt i, VInt j) -> VInt (i+j)
            | (Add, VAddr a, VInt i) -> VAddr (addr_offset a i)
            | (Add, VInt i, VAddr a) -> VAddr (addr_offset a i)
            | (Add, x, y) -> jir_error ("cannot add " ^ string_of_value x 
                                        ^ " to "
                                        ^ string_of_value y)
            | (Sub, VInt i, VInt j) -> VInt (i-j)
            | (Sub, VAddr a, VInt i) -> VAddr (addr_offset a (-i))
            | (Sub, VInt i, VAddr a) -> VAddr (addr_offset a (-i))
            | (Sub, x, y) -> jir_error ("cannot subtract " ^ string_of_value x 
                                        ^ " from " ^ string_of_value y)
            | (Eq, VInt i, VInt j) -> VInt (if i = j then 1 else 0)
            | (Eq, VAddr a, VAddr b) -> VInt (if addr_equal a b then 1 else 0)
            | (Neq, VInt i, VInt j) -> VInt (if i <> j then 1 else 0)
            | (Neq, VAddr a, VAddr b) -> 
                VInt (if addr_equal a b then 0 else 1)
            | (_, VInt i, VInt j) ->
                VInt (
                  match op with
                    | Mult -> i * j
                    | Div -> i / j
                    | And -> if i <> 0 && j <> 0 then 1 else 0
                    | Or -> if i = 0 && j = 0 then 0 else 1
                    | Lt -> if i < j then 1 else 0
                    | Leq -> if i <= j then 1 else 0
                    | Gt -> if i > j then 1 else 0
                    | Geq -> if i >= j then 1 else 0
                    | _ -> failwith ("ERROR: unexpected operator " 
                                     ^ string_of_op op)
                )
            | (_, x, y) -> jir_error ("operation " ^ string_of_op op 
                                      ^ " not applicable to " ^ string_of_value x
                                      ^ " and " ^ string_of_value y)
        end
    | Unop(op, e) ->
        match eval_exp mach e with
          | VInt i ->
              VInt (match op with
                        | UMinus -> -i
                        | Not -> if i = 0 then 1 else 0)
          | x -> jir_error ("unary operation " ^ string_of_unop op 
                            ^ " not applicable to " ^ string_of_value x)

  let exec_stmt mach ctrl label_to_pc = function
    | MoveMem(e1, e2) ->
        begin
          match (eval_exp mach e1, eval_exp mach e2) with
            | (VAddr a, v) -> (set_mem mach a v, inc_pc ctrl)
            | (x, _) -> jir_error ("invalid left operand " ^ string_of_value x 
                                   ^ " of MoveMem")
        end
    | MoveReg(r, e) ->
        let v = eval_exp mach e in
          (set_reg mach r v, inc_pc ctrl)
    | Call(r, i, es) ->
        begin
          match List.map (eval_exp mach) es with
            | [] -> jir_error ("invalid Call instruction: argument list empty")
            | (VAddr a :: _) as vs ->
                let (entry_label, arg_regs) = lookup_method mach a i in
                let new_pc = List.assoc entry_label label_to_pc in
                  if List.length arg_regs <> List.length vs
                  then jir_error ("invalid Call instruction: argument mismatch")
                  else 
                    let _ = debug ("Call: " ^ 
                                     string_of_list string_of_value vs) in
                    let ctrl' = enter_proc mach ctrl r new_pc in
                    let mach' = List.fold_left2 set_reg mach arg_regs vs in
                      (mach', ctrl')
            | (x :: _) ->
                jir_error ("invalid first argument for Call instruction: "
                           ^ string_of_value x)
        end
    | Syscall(r, Write, [e]) ->
        begin
          match eval_exp mach e with
            | VAddr a -> 
                let s = lookup_string mach a in
                let _ = print_string s in
                  (set_reg mach r (VInt 0), inc_pc ctrl)
            | VInt i ->
                let _ = print_int i in
                  (set_reg mach r (VInt 0), inc_pc ctrl)
        end
    | Syscall(r, Read, []) ->
        let s = read_line () in
        let (a, mach') = alloc_string mach s in
          (set_reg mach' r (VAddr a), inc_pc ctrl)
    | Syscall(r, Alloc, [e]) ->
        begin
          match eval_exp mach e with
            | VInt i ->
                let (a, mach') = alloc_mem mach i in
                  (set_reg mach' r (VAddr a), inc_pc ctrl)
            | x -> jir_error ("invalid argument for Alloc syscall: "
                              ^ string_of_value x)
        end
    | Syscall(_, Exit, []) ->
        raise Stop
    | Syscall(_, c, args) ->
        jir_error ("invalid number of arguments (" 
                   ^ string_of_int (List.length args)
                   ^ " for syscall " ^ string_of_syscall c)
    | Jump(e, l) ->
        begin
          match eval_exp mach e with
            | VInt 0 ->
                let _ = debug "jumping" in
                let new_pc = List.assoc l label_to_pc in
                  (mach, set_pc ctrl new_pc)
            | x -> 
                let _ = debug ("not jumping, value = " ^ string_of_value x) in
                  (mach, inc_pc ctrl)
        end
    | Return(e) ->
        let v = eval_exp mach e in
        let _ = debug ("returning with " ^ string_of_value v) in
        let (r, mach', ctrl') = exit_proc mach ctrl in
        let mach'' = set_reg mach' r v in
          (mach'', ctrl')
    | Label(_) -> (mach, inc_pc ctrl)
          
  let prepare_labels code =
    let label_to_pc = ref [] in
      begin
        for i = 0 to Array.length code - 1 do
          match code.(i) with
            | Label(l) -> label_to_pc := (l, i+1) :: !label_to_pc
            | _ -> ()
        done;
        !label_to_pc
      end

  type code = stmt array

  let run mach code pc =
    let label_to_pc = prepare_labels code in
    let rec loop mach ((pc, _) as ctrl) =
      if pc = -1 then
        () (* exit *)
      else
        begin
          let stmt = 
            try code.(pc) with
                Invalid_argument(_) -> jir_error ("invalid program counter: "
                                                  ^ string_of_int pc)
          in
          let (mach', ctrl') = exec_stmt mach ctrl label_to_pc stmt in
            loop mach' ctrl'
        end
    in
    let ctrl = init_ctrl pc in
      try loop mach ctrl with
        | Stop -> ()
end


(* SAMPLE CODE *)

module IntMap = Map.Make(struct type t = int let compare = compare end)

module Jir_Test =
struct
  type reg = int
  type addr = (int * int) (* base offset *)
  type label = int

  let string_of_addr (b, off) = 
    "(" ^ string_of_int b ^ ", " ^ string_of_int off ^ ")"

  (* concrete types representing instructions *)
  type op   = Add | Sub | Mult | Div | And | Or | Lt | Leq | Gt | Geq | Eq | Neq
  type uop  = UMinus | Not
  type sys  = Write | Read | Alloc | Exit
  type exp  = Const of int
              | Addr of addr 
              | Reg of reg
              | Get of exp
              | Binop of exp * op * exp
              | Unop of uop * exp
  type stmt = MoveMem of exp * exp
              | MoveReg of reg * exp
              | Call of reg * int * exp list
              | Syscall of reg * sys * exp list
              | Jump of exp * label
              | Label of label
              | Return of exp

  (* type synonyms *)
  type pc = int   (* the program counter *)
  type offset = int

  (* values *)
  type value = VInt of int | VAddr of addr
  let string_of_value = function
    | VInt(i) -> "VInt(" ^ string_of_int i ^ ")"
    | VAddr(a) -> "VInt(" ^ string_of_addr a ^ ")"

  (* vtbl and values stored in memory *)
  type vtbl_entry = { entry_label : label; arg_regs: reg list }
  type vtbl = vtbl_entry array

  type mem_value = MVArr of (value array) | MVString of string | MVVtbl of vtbl

  (* memory and register access *)
  let next_free_mem = ref 0

  type mach = { regs : value IntMap.t;
                mem : mem_value IntMap.t }

  let init_mach regs mem =
    let insert_list m l = 
      snd (List.fold_left (fun (k,m) v -> (k+1, IntMap.add k v m)) (0, m) l)
    in
    let reg_map = insert_list IntMap.empty regs in
    let mem_map = insert_list IntMap.empty mem in
      { regs = reg_map; mem = mem_map }

  let lookup_mem mach (b, off) = 
    match IntMap.find b mach.mem with
      | MVArr(arr) -> arr.(off)
      | _ -> failwith "illegal memory access"
  let lookup_reg mach r = IntMap.find r mach.regs
  let set_mem mach (b, off) v = 
    match IntMap.find b mach.mem with
      | MVArr(arr) -> arr.(off) <- v; mach
      | _ -> failwith "illegal memory access"
  let set_reg mach r v = { mach with regs = IntMap.add r v mach.regs }

  (* lookup functions for methods, strings, and labels *)
  let lookup_method mach (b, off) i =
    match IntMap.find b mach.mem with
      | MVVtbl(vtbl) ->
          let entry = vtbl.(i) in
            (entry.entry_label, entry.arg_regs)
      | _ -> failwith "illegal memory access"
  let lookup_string mach (b, off) = 
    if off = 0 then
      match IntMap.find b mach.mem with
        | MVString(s) -> s
        | _ -> failwith "illegal memory access"
    else
      failwith "indexed string access disallowed"

  (* memory allocation *)
  let alloc_string mach s =
    let i = !next_free_mem in
    let _ = next_free_mem := i+1 in
      ((i, 0), { mach with mem = IntMap.add i (MVString s) mach.mem })
  let alloc_mem mach len =
    let i = !next_free_mem in
    let _ = next_free_mem := i+1 in
    let arr = Array.make len (VInt 0) in
      ((i, 0), { mach with mem = IntMap.add i (MVArr arr) mach.mem })

  (* address manipulation *)
  let addr_offset (b, off) i = (b, off+i)
  let addr_equal a1 a2 = a1 = a2

  (* dumping and restoring registers *)
  type reg_dump = value IntMap.t
  let dump_registers mach = mach.regs
  let restore_registers mach regs = { mach with regs = regs }
end

module Jir_Test_Eval = Jir_Gen_Eval(Jir_Test)

let test_fak () =
  let vtbl_addr = (0,0) in
  let int_addr = Jir_Test.Addr(1,0) in
  let string_addr = Jir_Test.Addr(2,0) in
  let newline_addr = Jir_Test.Addr(3,0) in
  let regs = [Jir_Test.VAddr(vtbl_addr)] in
  let obj_reg = Jir_Test.Reg(0) in
  let return_reg = 1 in
  let dummy_reg = 2 in
  let arg_reg = 3 in
  let this_arg_reg = 4 in
  let fak_exit_label = 0 in
  let fak_entry_label = 1 in
  let fak_vtbl_entry = { Jir_Test.entry_label = fak_entry_label; 
                         Jir_Test.arg_regs = [this_arg_reg; arg_reg] } in
  let vtbl = Array.make 1 fak_vtbl_entry in
  let mem = [Jir_Test.MVVtbl vtbl; 
             Jir_Test.MVArr (Array.make 1 (Jir_Test.VInt 5)); 
             Jir_Test.MVString "fak(5) = ";
             Jir_Test.MVString "\n"] in
  let code = Array.of_list
    [Jir_Test.Call(return_reg, 0, 
                   [obj_reg; Jir_Test.Get(int_addr)]);(* return_reg := fak(5) *)
     Jir_Test.Syscall(dummy_reg, Jir_Test.Write, [string_addr]);
     Jir_Test.Syscall(dummy_reg, Jir_Test.Write, [Jir_Test.Reg(return_reg)]);
     Jir_Test.Syscall(dummy_reg, Jir_Test.Write, [newline_addr]);
     Jir_Test.Syscall(dummy_reg, Jir_Test.Exit, []);
     (* start of fak *)
     Jir_Test.Label(fak_entry_label);
     Jir_Test.Jump(Jir_Test.Reg(arg_reg), fak_exit_label);
     (* arg_reg > 0 *)
     (* return_reg = fak(arg_reg - 1) *)
     Jir_Test.Call(return_reg, 0, 
                   [obj_reg; Jir_Test.Binop(Jir_Test.Reg(arg_reg), 
                                            Jir_Test.Sub, 
                                            Jir_Test.Const(1))]);
     Jir_Test.Return(Jir_Test.Binop(Jir_Test.Reg(return_reg), 
                                    Jir_Test.Mult, 
                                    Jir_Test.Reg(arg_reg)));
     (* arg_reg = 0 *)
     Jir_Test.Label(fak_exit_label);
     Jir_Test.Return(Jir_Test.Const(1))]
  in
  let mach = Jir_Test.init_mach regs mem in
    Jir_Test_Eval.run mach code 0

let _ = test_fak ()
