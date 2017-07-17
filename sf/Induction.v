Require Export Basics.

Theorem plus_n_0 : forall n : nat, n = n + 0.
Proof. intros. induction n as [| n' IHn'].
       - reflexivity.
       - simpl. rewrite <- IHn'. reflexivity. Qed.

Theorem minus_diag : forall n, minus n n = 0.
Proof. intros. induction n as [| n' IHn].
       - reflexivity.
       - simpl. rewrite -> IHn. reflexivity. Qed.

Theorem mult_0_r : forall n : nat, n * 0 = 0.
Proof. intros. induction n as [| n' IH].
       - reflexivity.
       - simpl. rewrite -> IH. reflexivity. Qed.

Theorem plus_n_Sm : forall n m : nat, S (n + m) = n + S m.
Proof. intros. induction n as [| n' IH ].
       - reflexivity.
       - simpl. rewrite -> IH. reflexivity. Qed.

Theorem plus_comm : forall n m : nat, n + m = m + n.
Proof. intros. induction n as [| n' IH].
       - simpl. rewrite <- plus_n_0. reflexivity.
       - simpl. rewrite -> IH. rewrite plus_n_Sm. reflexivity. Qed.

Theorem plus_assoc : forall n m p : nat, n + (m + p) = (n + m) + p.
Proof. intros. induction n as [| n' IH ].
       - reflexivity.
       - simpl. rewrite -> IH. reflexivity. Qed.

Fixpoint double (n:nat) :=
  match n with
  | O => O
  | S n' => S (S (double n'))
  end.

Lemma double_plus : forall n, double n = n + n.
Proof. intros. induction n as [| n' IH ].
       - reflexivity.
       - simpl. rewrite <- plus_n_Sm. rewrite <- IH. reflexivity. Qed.

Theorem evenb_S : forall n : nat, evenb (S n) = negb (evenb n).
Proof. intros. induction n as [| n' IH ].
       -  reflexivity.
       - rewrite -> IH. simpl. rewrite -> negb_involutive. reflexivity. Qed.

Theorem mult_0_plus' : forall n m : nat,
    (0 + n) * m = n * m.
Proof. intros.
       assert (H: 0 + n = n). { reflexivity. }
       rewrite -> H.
       reflexivity.
Qed.

Theorem plus_rearrange : forall n m p q : nat,
  (n + m) + (p + q) = (m + n) + (p + q).
Proof.
  intros n m p q.
  assert (H: n + m = m + n).
  { rewrite -> plus_comm. reflexivity. }
  rewrite -> H. reflexivity. Qed.

(*
Translate your solution for plus_comm into an informal proof:
Theorem: Addition is commutative.
Proof: 

Write an informal proof of the following theorem, using the informal proof of plus_assoc as a model. Don't just paraphrase the Coq tactics into English!
Theorem: true = beq_nat n n for any n.
Proof: 

 *)


Theorem plus_swap : forall n m p : nat,
  n + (m + p) = m + (n + p).
Proof.
  intros.
  rewrite -> plus_assoc.
  assert (n + m = m + n) as H.
  { rewrite -> plus_comm. reflexivity. }
  rewrite H.
  rewrite -> plus_assoc.
  reflexivity.
Qed.

Theorem mult_succ : forall m n : nat,
  m * S n = m + m * n.
Proof.
  intros.
  induction m as [| m' IH ].
  - reflexivity.
  - simpl. rewrite -> IH. rewrite <- plus_swap. reflexivity.
Qed.
  
Theorem mult_comm : forall m n : nat,
  m * n = n * m.
Proof.
  intros.
  induction n as [| n' IH ].
  (* m * 0 = 0 * m *)
  - simpl. rewrite mult_0_r. reflexivity.
  (* m * S n' = S n' * m *)
  - simpl. rewrite mult_succ. rewrite IH. reflexivity.
Qed.

Theorem leb_refl : forall n:nat,
  true = leb n n.
Proof.
  intros.
  induction n as [| n' IH ].
  - simpl. reflexivity.
  - simpl. rewrite <- IH. reflexivity.
Qed.

Theorem zero_nbeq_S : forall n:nat,
  beq_nat 0 (S n) = false.
Proof.
  intros. induction n as [| n' IH ].
  - simpl. reflexivity.
  - simpl. reflexivity.
Qed.

Theorem andb_false_r : forall b : bool,
  andb b false = false.
Proof.
  intros. destruct b.
  - simpl. reflexivity.
  - simpl. reflexivity.
Qed.

(*
Theorem plus_ble_compat_l : forall n m p : nat,
  leb n m = true -> leb (p + n) (p + m) = true.
Proof.
  intros.
  induction p as [| p' IH ].
  - simpl. rewrite -> H. reflexivity.
  - simpl.
Qed.
*)

Theorem S_nbeq_0 : forall n:nat,
  beq_nat (S n) 0 = false.
Proof.
  intros.
  reflexivity.
Qed.

Theorem mult_1_l : forall n:nat, 1 * n = n.
Proof.
  intros.
  simpl. rewrite plus_n_0. reflexivity.
Qed.

Theorem all3_spec : forall b c : bool,
    orb
      (andb b c)
      (orb (negb b)
               (negb c))
  = true.
Proof.
  intros.
  destruct b.
  - simpl. destruct c.
    + reflexivity.
    + reflexivity.
  - simpl. reflexivity.
Qed.

Theorem mult_plus_distr_r : forall n m p : nat,
  (n + m) * p = (n * p) + (m * p).
Proof.
  intros.
  induction n as [| n' IH ].
  - reflexivity.
  - simpl. rewrite IH. rewrite plus_assoc. reflexivity.
Qed.                                      

Theorem mult_assoc : forall n m p : nat,
  n * (m * p) = (n * m) * p.
Proof.
  intros.
  induction n as [| n' IH ].
  - reflexivity.
  - simpl. rewrite -> mult_plus_distr_r. rewrite <- IH. reflexivity.
Qed.

Theorem beq_nat_refl : forall n : nat,
  true = beq_nat n n.
Proof.
  intros.
  induction n.
  - simpl. reflexivity.
  - simpl. apply IHn.
Qed.
  
(* TODO *)

 
(* Little Gauss' sum proof *)
(*
Fixpoint row_sum (n : nat) : nat :=
  match n with
  | O => O
  | S n' => n + (row_sum n')
  end.

Compute (row_sum 10).

Lemma sub_0 :  forall n : nat, n - 0 = n.
Proof.
  intros.
  induction n as [| n' IH ].
  - reflexivity.
  - simpl. reflexivity.
Qed.

Lemma add_0 : forall n : nat, n + 0 = n.
Proof.
  intros.
  induction n as [| n' IH ].
  - reflexivity.
  - simpl. rewrite IH. reflexivity.
Qed.

Lemma s_n_add_m : forall n m : nat, S(n + m) = S n + m.
Proof. intros. reflexivity. Qed.

Lemma double_n : forall n : nat, n + n = 2 * n.
Proof. intros.
       induction n as [| n' IH ].
       - reflexivity.
       - simpl. rewrite add_0. reflexivity.
Qed.

Lemma mult_s : forall n m : nat, n * S m = n * m + n.
Proof. intros.
       assert (SN : S m = m + 1).
       { induction m as [| m' IH].
         - reflexivity.
         - simpl. rewrite IH. reflexivity. }
         rewrite  SN. rewrite mult_comm. rewrite mult_plus_distr_r.
       simpl. rewrite mult_comm. rewrite add_0.
       reflexivity.
Qed.

Lemma mult_minus_distr_r : forall n m p : nat,
  p * (n - m) = (p * n) - (p * m).
Proof.
  intros.
  induction m as [| m' IH ].
  - rewrite sub_0. rewrite mult_0_r. rewrite sub_0. reflexivity.
  - rewrite mult_s.

Theorem row_sum_short :
  forall n : nat, 2 * (row_sum n) = n * (n - 1).

*)