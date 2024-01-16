(** * Utilities for graded exercise 3. *)
(** This file contains definitions that are mostly copied from chapter 9:
    - arithmetic expressions
    - boolean expressions
    - Definition of [state] and [update].
      The [update] operation is defined plymorphically here to allow
      its reuse for the function tables.

 *)
Require Import SfLib.
(** These are the definitions for arithmetic and boolean expressions
of the Imp language. They are copied from Chapter 9 *)

Inductive aexp : Type := 
  | ANum : nat -> aexp
  | AId : id -> aexp                
  | APlus : aexp -> aexp -> aexp
  | AMinus : aexp -> aexp -> aexp
  | AMult : aexp -> aexp -> aexp.
Hint Constructors aexp.

Inductive bexp : Type := 
  | BTrue : bexp
  | BFalse : bexp
  | BEq : aexp -> aexp -> bexp
  | BLe : aexp -> aexp -> bexp
  | BNot : bexp -> bexp
  | BAnd : bexp -> bexp -> bexp.
Hint Constructors bexp.


(** This is the definition of a [mapping] and [update] operations on it.
    It is a polymorphic version of chapter 9 state  *)
Definition mapping (T : Type) := id -> T.

Definition empty {T : Type} (null : T) : mapping T := 
  fun _ => null.

Definition update {T : Type} (st : mapping T) (X:id) (n : T) : mapping T :=
  fun X' => if beq_id X X' then n else st X'.

Axiom update_eq : forall T (n:T) X st,
                    (update st X n) X = n.

Axiom update_neq :
  forall T V2 V1 (n:T) st,
    beq_id V2 V1 = false ->
    (update st V2 n) V1 = (st V1).

Axiom update_shadow :
  forall T x1 x2 k1 k2 (f : mapping T),
    (update  (update f k2 x1) k2 x2) k1 = (update f k2 x2) k1.

Axiom update_same : forall T x1 k1 k2 (f : mapping T),
                      f k1 = x1 ->
                      (update f k1 x1) k2 = f k2.

Axiom update_permute :
  forall T (x1 x2: T) k1 k2 k3 f,
    beq_id k2 k1 = false -> 
    (update (update f k2 x1) k1 x2) k3 = (update (update f k1 x2) k2 x1) k3.

(** Now the definition of [state]:  *)
Definition state := mapping nat.
Definition empty_state := empty 0.

Fixpoint aeval (st : state) (e : aexp) : nat :=
  match e with
  | ANum n => n
  | AId X => st X 
  | APlus a1 a2 => (aeval st a1) + (aeval st a2)
  | AMinus a1 a2  => (aeval st a1) - (aeval st a2)
  | AMult a1 a2 => (aeval st a1) * (aeval st a2)
  end.

Fixpoint beval (st : state) (e : bexp) : bool :=
  match e with 
  | BTrue       => true
  | BFalse      => false
  | BEq a1 a2   => beq_nat (aeval st a1) (aeval st a2)
  | BLe a1 a2   => ble_nat (aeval st a1) (aeval st a2)
  | BNot b1     => negb (beval st b1)
  | BAnd b1 b2  => andb (beval st b1) (beval st b2)
  end.

Definition relation2 (X Y : Type):= X -> Y -> X -> Y -> Prop.
Inductive multi {X Y:Type} (R: relation2 X Y) : relation2 X Y:=
  | multi_refl  : forall (x : X) (y : Y), multi R x y x y
  | multi_step : forall (x y z : X) (x' y' z' : Y),
                    R x x' y y' ->
                    multi R y y' z z' ->
                    multi R x x' z z'.

Theorem multi_R : forall (X Y:Type) (R:relation2 X Y) (x y : X) (x' y' : Y),
       R x x' y y' -> (multi R) x x' y y'.
Proof.
  intros X Y R x y x' y' H.
  apply multi_step with y y'. apply H. apply multi_refl.   Qed.

(** The crucial properties of the [multi R] relation are 
       - [multi R] is reflexive
       - [multi R] is transitive
       - [multi R] relates everything related by [R] *)

Theorem multi_trans :
  forall (X X':Type) (R: relation2 X X') (x y z : X) (x' y' z' : X'),
      multi R x x' y y' ->
      multi R y y' z z' ->
      multi R x x' z z'.
Proof.
  intros X X' R x y z x' y' z' G H.
  multi_cases (induction G) Case.
    Case "multi_refl". assumption.
    Case "multi_step". 
      apply multi_step with y y'. assumption. 
      apply IHG. assumption.  Qed.

(** Hoare logic for Imp  *)
Definition Assertion := state -> Prop.
Definition assert_implies (P Q : Assertion) : Prop :=
  forall st, P st -> Q st.
Notation "P ~~> Q" := (assert_implies P Q) (at level 80).
Notation "P <~~> Q" := (P ~~> Q /\ Q ~~> P) (at level 80).

Definition assn_sub X a Q : Assertion :=
  fun (st : state) =>
    Q (update st X (aeval st a)).

Definition bassn b : Assertion :=
  fun st => (beval st b = true).

Lemma bexp_eval_true : forall b st,
  beval st b = true -> (bassn b) st.
Proof.
  intros b st Hbe.
  unfold bassn. assumption.  Qed.

Lemma bexp_eval_false : forall b st,
  beval st b = false -> ~ ((bassn b) st).
Proof.
  intros b st Hbe contra.
  unfold bassn in contra.
  rewrite -> contra in Hbe. inversion Hbe.  Qed.

