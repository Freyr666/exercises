Require Export Poly.

Theorem silly1 : forall(n m o p : nat),
    n = m ->
    [n;o] = [n;p] ->
    [n;o] = [m;p].
Proof.
  intros. rewrite <- H. apply H0.
Qed.

Theorem silly2 : forall(n m o p : nat),
     n = m ->
     (forall(q r : nat), q = r -> [q;o] = [r;p]) ->
     [n;o] = [m;p].
Proof.
  intros n m o p eq1 eq2.
  apply eq2. apply eq1.
Qed.

Theorem silly_ex :
  (forall n, evenb n = true -> oddb (S n) = true) ->
  evenb 3 = true ->
  oddb 4 = true.
Proof.
  intros eq1 eq2.
  apply eq1, eq2.
Qed.

Theorem silly3_firsttry : forall(n : nat),
    true = beq_nat n 5 ->
    beq_nat (S (S n)) 7 = true.
Proof.
  intros n H.
  simpl. symmetry. apply H.
Qed.

Theorem rev_exercise1 : forall(l l' : list nat),
    l = rev l' ->
    l' = rev l.
Proof.
  intros. rewrite H. symmetry. apply rev_involutive.
Qed.

Example trans_eq_example : forall(a b c d e f : nat),
     [a;b] = [c;d] ->
     [c;d] = [e;f] ->
     [a;b] = [e;f].
Proof.
  intros a b c d e f eq1 eq2.
  rewrite -> eq1. rewrite -> eq2. reflexivity. Qed.

Theorem trans_eq : forall(X:Type) (n m o : X),
  n = m -> m = o -> n = o.
Proof.
  intros X n m o eq1 eq2. rewrite -> eq1. rewrite -> eq2.
  reflexivity. Qed.

Example trans_eq_example' : forall(a b c d e f : nat),
     [a;b] = [c;d] ->
     [c;d] = [e;f] ->
     [a;b] = [e;f].
Proof.
  intros a b c d e f eq1 eq2.
  apply trans_eq with (m:=[c;d]).
  apply eq1. apply eq2.
Qed.

Example trans_eq_exercise : forall(n m o p : nat),
    m = (minustwo o) ->
    (n + p) = m ->
    (n + p) = (minustwo o).
Proof.
  intros n m o p eq1 eq2.
  apply trans_eq with m.
  apply eq2. apply eq1.
Qed.

Theorem S_injective : forall(n m : nat),
  S n = S m ->
  n = m.
Proof.
  intros n m H.
  inversion H.
  reflexivity.
Qed.

Theorem inversion_ex1 : forall(n m o : nat),
  [n; m] = [o; o] ->
  [n] = [m].
Proof.
  intros n m o H. inversion H. reflexivity. Qed.


Example inversion_ex3 : forall(X : Type) (x y z : X) (l j : list X),
  x :: y :: l = z :: j ->
  y :: l = x :: j ->
  x = y.
Proof.
  intros. inversion H0. reflexivity.
Qed.

Theorem beq_nat_0_l : forall n,
   beq_nat 0 n = true -> n = 0.
Proof.
  intros n.
  destruct n.
  - intros. reflexivity.
  - intros. inversion H.
Qed.


Example inversion_ex6 : forall(X : Type)
                         (x y z : X) (l j : list X),
    x :: y :: l = [] ->
    y :: l = z :: j ->
    x = z.
Proof.
  intros.
  inversion H.
Qed.

Theorem f_equal : forall(A B : Type) (f: A -> B) (x y: A),
  x = y -> f x = f y.
Proof.
  intros A B f x y H. rewrite H.
  reflexivity.
Qed.

Theorem S_inj : forall(n m : nat) (b : bool),
     beq_nat (S n) (S m) = b ->
     beq_nat n m = b.
Proof.
  intros n m b H. simpl in H. apply H.
Qed.

Theorem silly3' : forall(n : nat),
  (beq_nat n 5 = true -> beq_nat (S (S n)) 7 = true) ->
  true = beq_nat n 5 ->
  true = beq_nat (S (S n)) 7.
Proof.
  intros n eq H.
  symmetry in H.
  apply eq in H.
  symmetry. apply H.
Qed.

Theorem plus_n_n_injective : forall n m,
     n + n = m + m ->
     n = m.
Proof.
  intros n. induction n as [| n'].
  - intros. simpl in H. destruct m.
    + reflexivity.
    + inversion H.
  - intros. simpl in H. rewrite <- plus_n_Sm in H. destruct m.
    + inversion H.
    + simpl in H. rewrite <- plus_n_Sm in H. inversion H. apply IHn' in H1. rewrite H1. reflexivity.
Qed.

Theorem double_injective : forall n m,
     double n = double m ->
     n = m.
Proof.
  intros n. induction n as [| n'].
  - intros. destruct m.
    + reflexivity.
    + inversion H.
  - intros. destruct m.
    + inversion H.
    + apply f_equal. apply IHn'. inversion H. reflexivity.
Qed.

Theorem beq_nat_true : forall n m,
    beq_nat n m = true -> n = m.
Proof.
  intros n. induction n as [| n' IH ].
  (* n = 0 *)
  - intros. destruct m.
    + (* 0 = 0 *)   reflexivity.
    + (* 0 = S m *) inversion H.
  (* n -> S n *)
  - intros. destruct m.
    + (* S n' = 0 *)   inversion H.
    + (* S n' = S m *) simpl in H. apply IH in H. rewrite H. reflexivity.
Qed.

Theorem double_injective_take2 : forall n m,
     double n = double m ->
     n = m.
Proof.
  intros n m.
  (* n and m are both in the context *)
  generalize dependent n.
  (* Now n is back in the goal and we can do induction on
     m and get a sufficiently general IH. *)
  induction m as [| m'].
  - (* m = O *) simpl. intros n eq. destruct n as [| n'].
    + (* n = O *) reflexivity.
    + (* n = S n' *) inversion eq.
  - (* m = S m' *) intros n eq. destruct n as [| n'].
    + (* n = O *) inversion eq.
    + (* n = S n' *) apply f_equal.
      apply IHm'. inversion eq. reflexivity. Qed.

Theorem beq_id_true : forall x y,
  beq_id x y = true -> x = y.
Proof.
  intros [m] [n]. simpl. intros H.
  assert (H' : m = n). { apply beq_nat_true. apply H. }
  rewrite H'. reflexivity.
Qed.

Theorem nth_error_after_last: forall(n : nat) (X : Type) (l : list X),
    length l = n ->
    nth_error l n = None.
Proof.
  intros. generalize dependent n.
  induction l as [| h tl IH ].
  (* [] *)
  - intros. reflexivity.
  (* h :: tl *)
  - intros. simpl in H. destruct n.
    (* n = 0 *)
    + inversion H.
    (* n = S n' *)
    + simpl. inversion H. apply IH. reflexivity.
Qed.

Definition square n := n * n.

Lemma square_mult : forall n m, square (n * m) = square n * square m.
Proof.
  intros n m.
  unfold square.
  rewrite mult_assoc.
  assert (H : n * m * n = n * n * m).
  { rewrite mult_comm. rewrite mult_assoc. reflexivity. }
  rewrite H. rewrite mult_assoc.
  reflexivity.
Qed.


Definition sillyfun (n : nat) : bool :=
  if beq_nat n 3 then false
  else if beq_nat n 5 then false
  else false.

Theorem sillyfun_false : forall(n : nat),
  sillyfun n = false.
Proof.
  intros n. unfold sillyfun.
  destruct (beq_nat n 3).
    - (* beq_nat n 3 = true *) reflexivity.
    - (* beq_nat n 3 = false *) destruct (beq_nat n 5).
      + (* beq_nat n 5 = true *) reflexivity.
      + (* beq_nat n 5 = false *) reflexivity. Qed.

Theorem combine_split : forall X Y (l : list (X * Y)) l1 l2,
  split l = (l1, l2) ->
  combine l1 l2 = l.
Proof.
  intros X Y l.
  induction l as [| (x, y) l' ].
  - intros. simpl in H. inversion H. reflexivity.
  - simpl. destruct (split l') as [xs ys ]. intros.
    inversion H. simpl. assert (TLS : combine xs ys = l').
    apply IHl'. reflexivity. rewrite TLS. reflexivity.
Qed.

(* TODO Informal proof *)

Definition sillyfun1 (n : nat) : bool :=
  if beq_nat n 3 then true
  else if beq_nat n 5 then true
       else false.


Theorem sillyfun1_odd : forall(n : nat),
     sillyfun1 n = true ->
     oddb n = true.
Proof.
  intros n eq. unfold sillyfun1 in eq.
  destruct (beq_nat n 3) eqn:EQ3.
  - apply beq_nat_true in EQ3. rewrite EQ3. reflexivity.
  - destruct (beq_nat n 5) eqn:EQ5.
    + apply beq_nat_true in EQ5. rewrite EQ5. reflexivity.
    + inversion eq.
Qed.

Theorem bool_fn_applied_thrice :
  forall(f : bool -> bool) (b : bool),
    f (f (f b)) = f b.
Proof.
  intros. destruct (f b) eqn:B.
  - destruct b in B.
    + rewrite B, B. reflexivity.
    + destruct (f true) eqn:FTRUE.
      * rewrite FTRUE. reflexivity.
      * rewrite B. reflexivity.
  - destruct b in B.
    + destruct (f false) eqn:FFALSE.
      * rewrite B. reflexivity.
      * rewrite FFALSE. reflexivity.
    + rewrite B, B. reflexivity.
Qed.

Lemma beq_nat_n_n : forall n : nat, beq_nat n n = true.
Proof.
  intros.
  induction n as [| n' IH ].
  - simpl. reflexivity.
  - simpl. rewrite IH. reflexivity.
Qed.

Theorem beq_nat_sym : forall(n m : nat),
  beq_nat n m = beq_nat m n.
Proof.
  intros.
  destruct (beq_nat m n) eqn:MN.
  - apply beq_nat_true in MN. rewrite MN. apply beq_nat_n_n.
  - destruct (beq_nat n m) eqn:NM.
    + apply beq_nat_true in NM. rewrite NM in MN. inversion MN. symmetry. apply beq_nat_n_n.
    + reflexivity.
Qed.


(* TODO Informal proof FILL IN HERE *)

Theorem beq_nat_trans : forall n m p,
  beq_nat n m = true ->
  beq_nat m p = true ->
  beq_nat n p = true.
Proof.
  intros.
  apply beq_nat_true in H.
  apply beq_nat_true in H0.
  symmetry in H0. rewrite H, H0.
  apply beq_nat_n_n.
Qed.

Definition split_combine_statement : Prop :=
  forall X (l1 l2 : list X),
    length l1 = length l2 ->
    split (combine l1 l2) = (l1, l2).

Theorem split_combine : split_combine_statement.
Proof.
  intros X l1. induction l1 as [| x xs ].
  - intros l2. destruct l2 as [| y ys ].
    + intros. reflexivity.
    + intros. inversion H.
  - intros l2 H. destruct l2 as [| y ys ].
    + inversion H.
    + simpl. rewrite -> IHxs. reflexivity. inversion H. reflexivity.
Qed.

Theorem filter_exercise : forall(X : Type) (test : X -> bool)
                           (x : X) (l lf : list X),
    filter test l = x :: lf ->
    test x = true.
Proof.
  intros X test x l lf.
  generalize dependent x.
  induction l as [| y ys IH ].
  - simpl. intros. inversion H.
  - simpl. destruct (test y) eqn:TY.
    + intros. inversion H. rewrite <- H1. apply TY.
    + intros. apply IH in H. apply H.
Qed.

Fixpoint forallb {X : Type} (f : X -> bool) (l : list X) : bool :=
  match l with
  | [] => true
  | x::xs => (f x) && (forallb f xs)
  end.

Fixpoint existsb {X : Type} (f : X -> bool) (l : list X) : bool :=
  match l with
  | [] => false
  | x::xs => if (f x)
            then true
            else (existsb f xs)
  end.

Definition existsb' {X : Type} (f : X -> bool) (l : list X) : bool := negb (forallb (fun x => negb (f x)) l).

Theorem existsb_existsb' : forall (X : Type) (test : X -> bool)
                             (l : list X),
    existsb test l = existsb' test l.
Proof.
  intros.
  induction l as [| x xs IH ].
  - reflexivity.
  - unfold existsb'. simpl. destruct (test x) eqn:TX.
    + reflexivity.
    + simpl. inversion IH. unfold existsb' in H0. rewrite <- H0. reflexivity.
Qed.
