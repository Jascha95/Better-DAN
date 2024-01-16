type fragment =
    StrFrag of string
  | RegFrag of int
  | ValFrag of int
  | LblFrag of int

type argument =
    RegArg of int
  | ValArg of int
  | LblArg of string

type cmd =
    Label of string
  | Instr of
      fragment list			(* instruction template *)
	* argument list			(* argument list *)
	* int list			(* def list *)
	* int list			(* use list *)
  | Move of fragment list
	* int * int			(* move instruction: target, source *)

(*conversion to assembly*)
let toAsm regmap cmd =
  let rec showInstr frags_args =
    match frags_args with
      ([], _) ->
	""
    | (StrFrag s :: frags, args) ->
	s ^ showInstr (frags, args)
    | (RegFrag i :: frags, RegArg r :: args) ->
	"$" ^ string_of_int (regmap r) ^ showInstr (frags, args)
    | (ValFrag i :: frags, ValArg v :: args) ->
	string_of_int v ^ showInstr (frags, args)
    | (LblFrag i :: frags, LblArg str :: args) ->
	str ^ showInstr (frags, args)
  in
  match cmd with
    Label (l) ->
      l^":"
  | Instr (frags, args, defs, uses) ->
      showInstr (frags, args)
  | Move (frags, rd, rs) ->
      showInstr (frags, [RegArg rd; RegArg rs])

(* precolored registers; MIPS specific; assuming standard calling convention *)
let register_00 = 00
let register_v0 = 02	(* result registers *)
let register_v1 = 03
let register_a0 = 04	(* argument registers *)
let register_a1 = 05
let register_a2 = 06
let register_a3 = 07
let register_sp = 29	(* stack pointer *)
let register_fp = 30	(* frame pointer *)
let register_ra = 31	(* return address *)

let register_LO = 32	(* special register for mult/div *)
let register_HI = 33	(* special register for mult/div *)

(* addressing modes *)
let rrrFrag instr =
  [StrFrag instr; StrFrag" "; RegFrag 0; StrFrag","; RegFrag 1; StrFrag","; RegFrag 2]

let rrFrag instr =
  [StrFrag instr; StrFrag" "; RegFrag 0; StrFrag","; RegFrag 1]

let rriFrag instr =
  [StrFrag instr; StrFrag" "; RegFrag 0; StrFrag","; RegFrag 1; StrFrag","; ValFrag 2]

let rirFrag instr =
  [StrFrag instr; StrFrag" "; RegFrag 0; StrFrag","; ValFrag 1; StrFrag"("; RegFrag 2; StrFrag")"]

let rFrag instr =
  [StrFrag instr; StrFrag" "; RegFrag 0]

let mFrag =
  [StrFrag "m "; RegFrag 0; StrFrag ","; RegFrag 1]

let lFrag instr =
  [StrFrag instr; StrFrag" "; LblFrag 0]

let rrlFrag instr =
  [StrFrag instr; StrFrag" "; RegFrag 0; StrFrag","; RegFrag 1; StrFrag","; LblFrag 2]

(* instruction fragments *)
let subuFrag = rriFrag "subu"
let subiFrag = rriFrag "subi"
let subiuFrag = rriFrag "subiu"
let subFrag = rrrFrag "add"
let addFrag = rrrFrag "add"
let adduFrag = rriFrag "addu"
let addiuFrag = rriFrag "addiu"
let addiFrag = rriFrag "addi"

let multFrag = rrFrag "mult"
let mfloFrag = rFrag "mflo"

let swFrag = rirFrag "sw"
let lwFrag = rirFrag "lw"

let jrFrag = rFrag "jr"

let jalFrag = lFrag "jal"
let jFrag = lFrag "j"

let beqFrag = rrlFrag "beq"

let nopFrag = [StrFrag "nop"]

(* instructions *)
let sub rd rs rt =
  Instr (subFrag, [RegArg rd; RegArg rs; RegArg rt], [rd], [rs; rt])
let subu rd rs vv =
  Instr (subuFrag, [RegArg rd; RegArg rs; ValArg vv], [rd], [rs])
let subi rd rs vv =
  Instr (subiFrag, [RegArg rd; RegArg rs; ValArg vv], [rd], [rs])
let subiu rd rs vv =
  Instr (subiuFrag, [RegArg rd; RegArg rs; ValArg vv], [rd], [rs])
let add rd rs rt =
  Instr (addFrag, [RegArg rd; RegArg rs; RegArg rt], [rd], [rs; rt])
let addu rd rs vv =
  Instr (adduFrag, [RegArg rd; RegArg rs; ValArg vv], [rd], [rs])
let addiu rd rs vv =
  Instr (addiuFrag, [RegArg rd; RegArg rs; ValArg vv], [rd], [rs])
let addi rd rs vv =
  Instr (addiFrag, [RegArg rd; RegArg rs; ValArg vv], [rd], [rs])

let mult rs rt =
  Instr (multFrag, [RegArg rs; RegArg rt], [register_LO], [rs; rt])
let mflo rd =
  Instr (mfloFrag, [RegArg rd], [rd], [register_LO])


let sw rs vv rt =
  Instr (swFrag, [RegArg rs; ValArg vv; RegArg rt], [], [rs; rt])
let lw rd vv rt =
  Instr (lwFrag, [RegArg rd; ValArg vv; RegArg rt], [rd], [rt])
let jr rt =
  Instr (jrFrag,
	 [RegArg rt],
	 [],
	 [register_v0; register_v1; rt])
let jalr rt =
  Instr (jrFrag,
	 [RegArg rt],
	 [register_v0; register_v1; register_ra],
	 [register_a0; register_a1; register_a2; register_a3; rt])

let m rd rs =
  Instr (mFrag, [RegArg rd; RegArg rs], [rd], [rs])

let jal lab =
  Instr (jalFrag,
	 [LblArg lab],
	 [register_v0; register_v1; register_ra],
	 [register_a0; register_a1; register_a2; register_a3])
let j lab =
  Instr (jFrag, [LblArg lab], [], [])

let beq rs rt lab =
  Instr (beqFrag, [RegArg rs; RegArg rt; LblArg lab], [], [rs; rt])

let nop =
  Instr (nopFrag, [], [], [])
