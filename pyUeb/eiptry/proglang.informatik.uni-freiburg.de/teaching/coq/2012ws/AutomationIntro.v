Require Le.
Require Arith.
(** * Short Introduction to Proof-Automation and Ltac  *)

(** ** Proofs are Programs *)

(** The theorem [forall (P Q : Prop), (P -> Q) -> P -> Q /\ P] may be
proven directly as a Gallina term: *)

Definition l_proof : forall (P Q : Prop), (P -> Q) -> P -> Q /\ P :=
fun P Q H Hp => conj (H Hp) Hp.

(** The more common way is to use tactics. The result is similar;
tactics merely are an interactive way to write proof terms (aka
programs) *)

Lemma l_tactic : forall P Q : Prop, (P -> Q) -> P -> Q /\ P.
Proof. (** construct a function with four arguments *)
       intros P Q H Hp.
       (** use [conj] *)
       split.
       (** first argument; [H] with argument [Hp] *)
       apply H. apply Hp.
       (** second argument *)
       apply Hp.
Qed.

(** The use of tactics in the above example correspond very closely to
the structure the proof term. Tactics like [induction] and [inversion]
do more work, also dependent on the current context; induction, for
example, can automatically introduce variables. However, all tactics
produce proof terms, that are then checked by Coq for consistency.

Most basic tactics, however, need to be told on what objects in the
current goal to operate. This can become cumbersome. It is also
fragile, as a lot of objects in a proof are named automatically and it
is practically infeasible to anticipate the naming in all cases. This
is especially a problem when some definitions have to be changed, even
if they do not really change the proof (e.g. reordering of arguments
in constructors); if one relies on explicit naming in the tactics the
old proofs will surely fail.
*)

(** ** The [auto] tactic  *)
(** One possibility to avoid cumbersome enumeration of every proof
step, and the problem of explicit naming of tactic arguments are the
automation tactics Coq provides. The most basic of these tactics is
[auto]. It will try to prove any goal using backwards-reasoning: it
examines the current goal and tries to find matching lemmas. Then it
applies them in turn and repeats until the goal is solved. If it
encounters a ``dead-end'' it backtracks to the last point where there
are still choices left; in other words, it's a depth-first search.
The lemmas that [auto] attempts to use are the hypothesis of the
current goal and predefined sets of lemmas called hint databases.  The
exact procedure and more details can be found in the Coq manual:

#<a href="http://coq.inria.fr/distrib/V8.4/refman/Reference-Manual011.html##toc64">
http://coq.inria.fr/distrib/V8.4/refman/Reference-Manual011.html##toc64
</a>#

Let's see how [auto] works by some simple examples (taken from
Software Foundations Chapter ``UseAuto'', which is available
#<a href="http://www.cis.upenn.edu/~bcpierce/sf/">here</a>#): *)

(** [auto] can do [reflexivity]  *)
Lemma solving_by_reflexivity : 
  2 + 3 = 5.
Proof. auto. Qed.

(** [auto] uses hypothesis  *)
Lemma solving_by_apply : forall (P Q : nat->Prop),
  (forall n, Q n -> P n) -> 
  (forall n, Q n) ->
  P 2.
Proof.
auto. Qed.
(** cf
[[
  intros P Q H H'. apply H. apply H'.
]]
 *)

(** [auto] has limited search depth. The following fairly trivial fact
is not solved by auto, because it by default only applies up to five
lemmas subsequently, before doing backtracking *)

Lemma search_depth_0 : 
  True /\ True /\ True /\ True /\ True /\ True.
Proof.
  auto. 
Admitted.

(** Supplying a numeric argument to [auto] can increase the search depth limit:  *)
Lemma search_depth_0' : 
  True /\ True /\ True /\ True /\ True /\ True.
Proof. auto 10. Qed.

(** ** Hints *)
(** [auto] only uses the hypothesis and some standard lemmas; it can
be extended though: *)

(** Here the lemma [le_n_S] has to be applied by hand.  *)

Lemma simple_le : forall n m P, P -> (P -> n <= m) -> S n <= S m.
  auto. intros. apply Le.le_n_S. auto.  Qed.

(** We can tell [auto] to include it specifically:  *)

Lemma simple_le' : forall n m, n <= m -> S n <= S m. auto using Le.le_n_S. Qed.

(** By the way, if one wants to see what proof [auto] comes up with,
the following form can be used *)

Lemma simple_le'' : forall n m, n <= m -> S n <= S m. info auto using Le.le_n_S. Qed.

(** Another possibility is to use Hints. [Hint Resolve <lemma>] will
make [auto] consider <lemma> for all subsequent proofs. *)

Hint Resolve Le.le_n_S.
Lemma simple_le''' : forall n m, n <= m -> S n <= S m. auto. Qed.

(** The same thing has to be done for constructors:  *)
Inductive someprop : nat -> Prop :=
| sp_5 : someprop 5
| sp_6 : someprop 6
| sp_sum : forall n, someprop n  -> someprop ( S (S (S n)))
.

(** [auto] fails, as it does not know about [sp_5].  *)
Example simple_someprop : someprop 5. auto. apply sp_5. Qed.

(** This works:  *)
Hint Resolve sp_5.
Example simple_someprop' : someprop 5. auto. Qed.

(** .. but only for one constructor.  *)

Example simple_someprop2 : someprop 6. auto. apply sp_6. Qed.

(** Here's a shortcut for adding all constructors of a type:  *)

Hint Constructors someprop.
Example simple_someprop2' : someprop 6. Print Hint. auto. Qed.

(** The Hints are that we added are all stored in a database called
[core]. There are also other predefined databases that define lemmas
for special purposes; a list of them can be found in the manual. There
it is also explained how to create your own databases and add your own
lemmas to them. The following is an example using the [arith]
database, which includes a lot of lemmas for calculation; it can do
similar things as [omega]. *)

Example use_arith_db : forall n, n + 1 = 1 + n.
auto.  auto with arith.  Qed.

(** It is possible to inspect the (current) content of a hint database
with the [Print Hint] command: *)

Print HintDb arith.

(** ** E-tactics  *)
(** The following simple fact is not solvable by auto.  *)
Lemma solving_by_eapply : forall (P Q : nat->Prop),
  (forall n m, Q m -> P n) ->
  Q 1 -> P 2.
Proof. auto. Admitted.

(** The reason is that it does not make any attempt to find a matching
term for [m] (which should be [1]). Compared with the manual version of the proof

[[
   intros P Q H H'. apply H with (m:=1). auto.
]]

one notes that [apply] only works by giving the value of [m]
explicitly. There is an alternative to [apply] called [eapply] that
postpones the need to instantiate [m]. 

[[
   intros P Q H H'. eapply H . apply H'. 
]]

It works by introducing so called ``existential variables''. They act
like wildcards, that have to be instantiated later in the proof. In
the above case, it becomes clear that [m] needs to match with [1] in
the next step. Existential variables are prefixed in the proof state
with a question mark; try out the above example to see how it looks and works.

The tactic [eauto] always uses [eapply] instead of [apply] and
therefore is able to solve the simple fact from above:
 *)

Lemma solving_by_eapply' : forall (P Q : nat->Prop),
  (forall n m, Q m -> P n) ->
  Q 1 -> P 2.
Proof. eauto. Qed.

(** [eauto] has also some drawbacks; it is slower than [auto] as it
has more matching possibilities due to the existential
variables... and it may still fail, when it makes wrong choices about
instantiations.  It is therefore preferable to use [eauto] only in
situations where the possibilities for the existentials are rather
constrained; e.g. in the example above, there was only one
possibility. *)

(** ** Ltac  *)
(** In addition to the tactics that Coq provide out of the box, it
includes a quite powerful scripting language called Ltac, that allows
to program custom tactics using existing ones as building blocks.

You are encouraged to look at the Ltac manual at

#<a href="http://coq.inria.fr/distrib/V8.4/refman/Reference-Manual012.html">
http://coq.inria.fr/distrib/V8.4/refman/Reference-Manual012.html
</a>#

for the exact syntax and semantics.

This is particularly useful for automating proofs as [auto] and
[eauto] have a significant shortcoming: they _only_ apply lemmas and
never do rewriting or inversion.

For example, most of the times when dealing with negation, inversion is key; like in the following example: *)

(** [auto] cannot do inversions  *)
Lemma not_someprop: ~ (someprop 7).
Proof. unfold not. intros. auto. (** [auto] can't do anything  *)
       inversion H. subst. inversion H1. subst. inversion H2. Qed.
(** so we do cumbersome and fragile inversions by hand.  *)

(** Examining the manual proof above, we can notice we inversion
always on the freshest [someprop] result in the hypothesis. Let us
define a tactic using Ltac that will do that for us: *)

Ltac invert_someprop :=
       match goal with
         | [H : someprop _ |- _ ] =>
           inversion H; clear H;  subst; simpl
       end.

(** Here, we define a new tactic [invert_someprop]. The special match
form [match goal with ...] checks if the current proof state matches
the ``goal patterns'' in its body and then executes the corresponding
tactics. Check the manual for the exact goal pattern syntax.

In our case we match for a hypothesis that is a statement about
[someprop] and bind it to the name [H] (this is the [H : someprop _]
part). We also accept any goal (the [ |- _ ] part) and then use
[inversion] on the bound name [H]. The following tactics clean up the
proof state a bit. If you do not know them yet, you can read up on
them in the Coq manual.

#<a href="http://coq.inria.fr/distrib/V8.4/refman/Reference-Manual011.html">
http://coq.inria.fr/distrib/V8.4/refman/Reference-Manual011.html
</a>#

The semicolon between the tactics simply applies the right tactic to
all the subgoals that the left tactic generated.

Let's see how our custom tactic works:
 *)

Lemma not_someprop': ~ (someprop 7).
Proof. unfold not. intros. invert_someprop. invert_someprop. invert_someprop. Qed.

(** We now always match the right thing to invert without having to
state it's auto-generated name. However, we still have to manually
apply the tactic three times.

To fix this we will use a so called ``tactical''. A tactical can be
viewed as a control structure of Ltac. One example is the semicolon
from our definition of [invert_someprop]. Here we can use another one,
[repeat], that will repeat a tactic until it either fails or solves
the goal.
 *)
Lemma not_someprop'': ~ (someprop 7).
Proof. unfold not. intros. repeat invert_someprop. Qed.

(* This makes our proof much shorter and more robust; it basically will work on all [someprop]s with a literal argument: *)
Lemma not_someprop14: ~ (someprop 19).
Proof. unfold not. intros. repeat invert_someprop. Qed.

Lemma not_someprop101: ~ (someprop 106).
Proof. unfold not. intros. repeat invert_someprop. Qed.

(** If we want to use the invert_someprop  tactic implicitly, we can add it as a special external hing: *)

Hint Extern 4 => invert_someprop.

(** Now [auto] will always also try to use the [invert_somprop] tactic:  *)
Lemma not_someprop''': ~ (someprop 7).
Proof. auto. Qed.


(** Here's another example where automatic inversion of impossible
cases is useful *)

Inductive non_empty {X : Type}: list X -> Prop :=
| ne_constr: forall (x:X) l, non_empty (x::l).

Ltac invert_ne :=
match goal with
| [ H: non_empty nil |- _ ] => solve [inversion H]
end.
(** [solve] is a tactical that just executes its argument but also
fails, if the current goal was not solved *)

Hint Extern 4 => invert_ne.
Lemma llist: forall X (l: list X), non_empty l -> length l >= 1.
Proof. intros X l. induction l; simpl; auto with core arith.
Qed.


(** Another special matching construct that is often useful is
[context]. It may be used for matching hypothesis or goals and it
matches if its argument (which has to be given in brackets) merely
occurs in the goal, i.e. also subterms of the goals are matched

In the following example, we want to eliminate occurrences of plus
where the right hand side is a literal number (i.e. no variable). We
do this by first ``shoving'' all successor constructors to the left
using rewriting of [plus_n_Sm] and then by eliminating a leftover zero
on the right hand side with [plus_n_O]:
 *)

Ltac canonical_plus :=
  match goal with
    | [ |- context [ _ + S _ ] ] => rewrite <- plus_n_Sm
    | [ |- context [_ + O] ] => rewrite <- plus_n_O
  end.

Hint Extern 5 => repeat canonical_plus.
Lemma someprop_or_not : forall n, someprop (S n) -> someprop (n + 7).
Proof. 
intros;   auto. 
Qed.
  
(** Some final tips on getting started with automation:

    - start modest: try to automate little things at first; if you not
      know what you are doing you might get really unpredictable
      results

    - Don't spend to much time on automation if it does not work as
      expected; if you get stuck it's best to do the proof manually
      and then experiment to automate parts of it when you have time.

    - However, it is always beneficial to spend some extra keystrokes
      to use a simple goal match instead of writing the explicit
      auto-generated names in your tactics. That way you also will get
      good practice how matching really works (we just covered part of
      it here).

    - Always keep the Coq manual at hand and read the (unfortunately
      very concise) descriptions of the automation features that you
      use, when you encounter problems.

 *)
