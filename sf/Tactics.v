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

