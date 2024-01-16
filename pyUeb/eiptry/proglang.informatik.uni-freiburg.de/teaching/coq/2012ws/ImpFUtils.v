(** * ImpF smallstep semantics.  *)
Require Import SfLib.
Require Export ImpUtils.

Definition ACC : id := Id 0.

(** This is the abstract syntax of ImpF commands *)
Inductive com : Type :=
| CSkip : com
| CAss : id -> aexp -> com
| CSeq : com -> com -> com
| CIf : bexp -> com -> com -> com
| CWhile : bexp -> com -> com
| CCall : id -> id -> aexp -> com.
Hint Constructors com.

(** Now function definitions and programs:  *)
Inductive def : Type :=
| Def : id -> id -> com -> def.
Hint Constructors def.

Inductive prog : Type :=
| Prog : list def -> com -> prog.
Hint Constructors prog.

(** The following adds a notation for the CALL command:  *)
Notation "X '::=' 'CALL' f '[[' a ']]'" :=
  (CCall X f a) (at level 60, right associativity).

(** These notations let us write ImpF programs like in the example above:  *)
Notation "'DEF' id '[[' X ']]' 'BEGIN' c 'END'" :=
  (Def id X c) (at level 90).
Notation "ds 'MAIN' 'BEGIN' c 'END'" :=
  (Prog ds c) (at level 90).

(** And we have to redefine the notations from chapter 7... sorry for the noise *)
Notation "'SKIP'" := 
  CSkip.
Notation "X '::=' a" := 
  (CAss X a) (at level 60).
Notation "c1 ; c2" := 
  (CSeq c1 c2) (at level 80, right associativity).
Notation "'WHILE' b 'DO' c 'END'" := 
  (CWhile b c) (at level 80, right associativity).
Notation "'IFB' e1 'THEN' e2 'ELSE' e3 'FI'" := 
  (CIf e1 e2 e3) (at level 80, right associativity).
Reserved Notation " tbl '|=' t '/' st '==>' t' '/' st' " 
         (at level 85, t at level 39, st at level 49, t' at level 39).

Definition funtbl : Type := mapping (option (id * com)%type).
Definition empty_funtbl : funtbl := empty None.
Definition lkup (tbl : funtbl) (f : id) (arg : id) (body : com) :=
  tbl f = Some (arg, body).

Inductive cstep (tbl : funtbl) : com -> state -> com -> state -> Prop :=
| CS_Ass : forall st i n a st',
             n = aeval st a ->
             st' = update st i n ->
             tbl |= (i ::= a) / st ==> SKIP / st'
| CS_SeqStep : forall st c1 c1' st' c2,
                 tbl |= c1 / st ==> c1' / st' ->
                 tbl |= (c1 ; c2) / st ==> (c1' ; c2) / st'
| CS_SeqFinish : forall st c2,
                   tbl |= (SKIP ; c2) / st ==> c2 / st
| CS_IfTrue : forall b st c1 c2,
                beval st b = true ->
                tbl |= (IFB b THEN c1 ELSE c2 FI) / st ==> c1 / st
| CS_IfFalse : forall b st c1 c2,
                 beval st b = false ->
                 tbl |= (IFB b THEN c1 ELSE c2 FI) / st ==> c2 / st
| CS_While : forall st b c1,
               tbl |=    (WHILE b DO c1 END) / st
                   ==> (IFB b THEN (c1; (WHILE b DO c1 END)) ELSE SKIP FI) / st
(** Fix the definition of [CS_Call]  *)
| CS_Call :
    forall st Y f a arg body n,
      lkup tbl f arg body ->
      n = aeval st a ->
      tbl |= (Y ::= CALL f[[a]]) / st ==> (arg ::= (ANum n); body; Y ::= (AId ACC)) / st
          where " tbl '|=' t '/' st '==>' t' '/' st' " := (cstep tbl t st t' st').

Definition multistep (tbl : funtbl) := multi (cstep tbl).
Notation "tbl '|=' c '/' st '==>*' c1 '/' st1 " :=
  (multistep tbl c st c1 st1) (at level 85, c at level 39, c1 at level 39
                               , st at level 49).