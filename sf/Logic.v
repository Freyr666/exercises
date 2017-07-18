Require Export Tactics.

Check 3 = 3.
(* ===> Prop *)

Check forall n m : nat, n + m = m + n.
(* ===> Prop *)

Definition injective {A B} (f : A -> B) :=
  forall x y : A, f x = f y -> x = y.

Lemma succ_inj : injective S.
Proof.
  intros n m H. inversion H. reflexivity.
Qed.

Example and_example : 3 + 4 = 7 /\ 2 * 2 = 4.
Proof.
  split.
  - reflexivity.
  - reflexivity.
Qed.

Lemma and_intro : forall A B : Prop, A -> B -> A /\ B.
Proof.
  intros A B HA HB. split.
  - apply HA.
  - apply HB.
Qed.

Example and_example' : 3 + 4 = 7 /\ 2 * 2 = 4.
Proof.
  apply and_intro.
  - (* 3 + 4 = 7 *) reflexivity.
  - (* 2 + 2 = 4 *) reflexivity.
Qed.

Example and_exercise :
  forall n m : nat, n + m = 0 -> n = 0 /\ m = 0.
Proof. intros. split.
       - destruct n.
         + reflexivity.
         + inversion H.
       - destruct m.
         + reflexivity.
         + rewrite plus_comm in H. inversion H.
Qed.

Lemma and_example2 :
  forall n m : nat, n = 0 /\ m = 0 -> n + m = 0.
Proof.
  (* WORKED IN CLASS *)
  intros n m H.
  destruct H as [Hn Hm].
  rewrite Hn. rewrite Hm.
  reflexivity.
Qed.

Lemma and_example3 :
  forall n m : nat, n + m = 0 -> n * m = 0.
Proof.
  intros n m H.
  assert (H' : n = 0 /\ m = 0).
  { apply and_exercise. apply H. }
  destruct H' as [Hn Hm].
  rewrite Hn. reflexivity.
Qed.

Lemma proj1 : forall P Q : Prop,
  P /\ Q -> P.
Proof.
  intros P Q [HP HQ].
  apply HP. Qed.

Lemma proj2 : forall P Q : Prop,
  P /\ Q -> Q.
Proof.
  intros P Q [HP HQ].
  apply HQ. Qed.

Theorem and_commut : forall P Q : Prop,
  P /\ Q -> Q /\ P.
Proof.
  (* WORKED IN CLASS *)
  intros P Q [HP HQ].
  split.
    - (* left *) apply HQ.
    - (* right *) apply HP. Qed.

Theorem and_assoc : forall P Q R : Prop,
  P /\ (Q /\ R) -> (P /\ Q) /\ R.
Proof.
  intros P Q R [HP [HQ HR]].
  split.
  - split.
    + apply HP.
    + apply HQ.
  - apply HR.
Qed.

Lemma or_example :
  forall n m : nat, n = 0 \/ m = 0 -> n * m = 0.
Proof.
  (* This pattern implicitly does case analysis on
     n = 0 ∨ m = 0 *)
  intros n m [Hn | Hm].
  - (* Here, n = 0 *)
    rewrite Hn. reflexivity.
  - (* Here, m = 0 *)
    rewrite Hm. rewrite <- mult_n_O.
    reflexivity.
Qed.

Lemma or_intro : forall A B : Prop, A -> A \/ B.
Proof.
  intros A B HA.
  left.
  apply HA.
Qed.

Lemma zero_or_succ :
  forall n : nat, n = 0 \/ n = S (pred n).
Proof.
  intros [|n].
  - left. reflexivity.
  - right. reflexivity.
Qed.

Lemma mult_eq_0 :
  forall n m, n * m = 0 -> n = 0 \/ m = 0.
Proof.
  intros n m H.
  destruct n.
  - left. reflexivity.
  - destruct m.
    + right. reflexivity.
    + inversion H.
Qed.
  
Theorem or_commut : forall P Q : Prop,
  P \/ Q -> Q \/ P.
Proof.
  intros P Q [HP | HQ].
  - right. apply HP.
  - left. apply HQ.
Qed.

Theorem ex_falso_quodlibet : forall(P:Prop),
  False -> P.
Proof.
  (* WORKED IN CLASS *)
  intros P contra.
  destruct contra. Qed.

Fact not_implies_our_not : forall(P:Prop),
  ~P -> (forall(Q:Prop), P -> Q).
Proof.
  intros P HNP Q HP.
  destruct HNP. apply HP.
Qed.

Theorem zero_not_one : ~(0 = 1).
Proof.
  intros contra. inversion contra.
Qed.

Theorem zero_not_one' : 0 <> 1.
Proof.
  intros H. inversion H.
Qed.

Theorem not_False :
  ~ False.
Proof.
  unfold not. intros H. destruct H. Qed.

Theorem contradiction_implies_anything : forall P Q : Prop,
  (P /\ ~P) -> Q.
Proof.
  (* WORKED IN CLASS *)
  intros P Q [HP HNA]. unfold not in HNA.
  apply HNA in HP. destruct HP. Qed.

Theorem double_neg : forall P : Prop,
  P -> ~~P.
Proof.
  (* WORKED IN CLASS *)
  intros P H. unfold not. intros G. apply G. apply H. Qed.

Theorem contrapositive : forall(P Q : Prop),
  (P -> Q) -> (~Q -> ~P).
Proof.
  intros P Q H. unfold not. intros HQ HP.
  apply HQ in H. destruct H. apply HP.
Qed.
  
Theorem not_both_true_and_false : forall P : Prop,
  ~ (P /\ ~P).
Proof.
  intros P.
  unfold not. intros [HP HNP]. apply HNP in HP. destruct HP.
Qed.

Theorem not_true_is_false : forall b : bool,
  b <> true -> b = false.
Proof.
  intros [] H.
  - (* b = true *)
    unfold not in H.
    apply ex_falso_quodlibet.
    apply H. reflexivity.
  - (* b = false *)
    reflexivity.
Qed.

Theorem not_true_is_false' : forall b : bool,
  b <> true -> b = false.
Proof.
  intros [] H.
  - (* b = false *)
    unfold not in H.
    exfalso. (* <=== *)
    apply H. reflexivity.
  - (* b = true *) reflexivity.
Qed.

Module MyIff.

  Definition iff (P Q : Prop) := (P -> Q) /\ (Q -> P).

  Notation "P <-> Q" := (iff P Q)
                        (at level 95, no associativity)
                      : type_scope.

End MyIff.


Theorem iff_sym : forall P Q : Prop,
  (P <-> Q) -> (Q <-> P).
Proof.
  (* WORKED IN CLASS *)
  intros P Q [HAB HBA].
  split.
  - (* -> *) apply HBA.
  - (* <- *) apply HAB. Qed.

Lemma not_true_iff_false : forall b,
  b <> true <-> b = false.
Proof.
  (* WORKED IN CLASS *)
  intros b. split.
  - (* -> *) apply not_true_is_false.
  - (* <- *)
    intros H. rewrite H. intros H'. inversion H'.
Qed.

Theorem iff_refl : forall P : Prop,
  P <-> P.
Proof.
  intros P. split.
  - intros. apply H.
  - intros. apply H.
Qed.
  
Theorem iff_trans : forall P Q R : Prop,
  (P <-> Q) -> (Q <-> R) -> (P <-> R).
Proof.
  intros P Q R [PQ QP] [QR RQ].
  split.
  - intros. apply PQ in H. apply QR in H. apply H.
  - intros. apply RQ in H. apply QP in H. apply H.
Qed.
Theorem or_distributes_over_and : forall P Q R : Prop,
  P \/ (Q /\ R) <-> (P \/ Q) /\ (P \/ R).
Proof.
  intros P Q R. split.
  - intros [HP | [HQ HR]].
    + split.
      * left. apply HP.
      * left. apply HP.
    + split.
      * right. apply HQ.
      * right. apply HR.
  - intros [[HPR | HQ] [HPL | HR]].
    + left. apply HPR.
    + left. apply HPR.
    + left. apply HPL.
    + right. split.
      * apply HQ.
      * apply HR.
Qed.

Require Import Coq.Setoids.Setoid.

Lemma mult_0 : forall n m, n * m = 0 <-> n = 0 \/ m = 0.
Proof.
  split.
  - apply mult_eq_0.
  - apply or_example.
Qed.

Lemma or_assoc :
  forall P Q R : Prop, P \/ (Q \/ R) <-> (P \/ Q) \/ R.
Proof.
  intros P Q R. split.
  - intros [H | [H | H]].
    + left. left. apply H.
    + left. right. apply H.
    + right. apply H.
  - intros [[H | H] | H].
    + left. apply H.
    + right. left. apply H.
    + right. right. apply H.
Qed.

Lemma mult_0_3 :
  forall n m p, n * m * p = 0 <-> n = 0 \/ m = 0 \/ p = 0.
Proof.
  intros n m p.
  rewrite mult_0. rewrite mult_0. rewrite or_assoc.
  reflexivity.
Qed.

Lemma apply_iff_example :
  forall n m : nat, n * m = 0 -> n = 0 \/ m = 0.
Proof.
  intros n m H. apply mult_0. apply H.
Qed.

Lemma four_is_even : exists n : nat, 4 = n + n.
Proof.
  exists 2. reflexivity.
Qed.

Theorem exists_example_2 : forall n,
  (exists m, n = 4 + m) ->
  (exists o, n = 2 + o).
Proof.
  (* WORKED IN CLASS *)
  intros n [m Hm]. (* note implicit destruct here *)
  exists(2 + m).
  apply Hm. Qed.

Theorem dist_not_exists : forall(X:Type) (P : X -> Prop),
  (forall x, P x) -> ~ (exists x, ~ P x).
Proof.
  intros.
  unfold not. intros NH. destruct NH. apply H0. apply H.
Qed.

Theorem dist_exists_or : forall (X:Type) (P Q : X -> Prop),
    (exists x, P x \/ Q x) <-> (exists x, P x) \/ (exists x, Q x).
Proof.
  intros X P Q. split.
  - intros [x [HP | HQ]].
    + left. exists x. apply HP.
    + right. exists x. apply HQ.
  - intros [[x HP] | [x HQ]].
    + exists x. left. apply HP.
    + exists x. right. apply HQ.
Qed.

Fixpoint In {A : Type} (x : A) (l : list A) : Prop :=
  match l with
  | [] => False
  | x' :: l' => x' = x \/ In x l'
  end.

Example In_example_1 : In 4 [1; 2; 3; 4; 5].
Proof.
  (* WORKED IN CLASS *)
  simpl. right. right. right. left. reflexivity.
Qed.

Example In_example_2 :
  forall n, In n [2; 4] ->
  exists n', n = 2 * n'.
Proof.
  (* WORKED IN CLASS *)
  simpl.
  intros n [H | [H | []]].
  - exists 1. rewrite <- H. reflexivity.
  - exists 2. rewrite <- H. reflexivity.
Qed.

Lemma In_map :
  forall(A B : Type) (f : A -> B) (l : list A) (x : A),
    In x l ->
    In (f x) (map f l).
Proof.
  intros A B f l x.
  induction l as [|x' l' IHl'].
  - (* l = nil, contradiction *)
    simpl. intros [].
  - (* l = x' :: l' *)
    simpl. intros [H | H].
    + rewrite H. left. reflexivity.
    + right. apply IHl'. apply H.
Qed.

Lemma In_map_iff :
  forall(A B : Type) (f : A -> B) (l : list A) (y : B),
    In y (map f l) <->
    exists x, f x = y /\ In x l.
Proof.
  intros A B f l y. split.
  - intros. induction l as [| x' l' IH].
    + simpl. inversion H.
    + simpl.
      inversion H. simpl in H.
      * exists x'. split.
        { apply H0. }
        { left. reflexivity. }
      * apply IH in H0.
        inversion H0. exists x. inversion H1. split.
        { apply H2. }
        { right. apply H3. }
  - intros. induction l as [| x' l' IH].
    + simpl. destruct H. inversion H. contradiction.
    + simpl. simpl in H. destruct H as [x'' H].
      inversion H. destruct H1 as [H2 | H3].
      { left. rewrite H2. apply H0. }
      { right. apply IH. exists x''. split.
        - apply H0.
        - apply H3.
      }
Qed.

Lemma in_app_iff : forall A l l' (a:A),
  In a (l++l') <-> In a l \/ In a l'.
Proof.
  intros. split.
  - induction l.
    { simpl. intros. right. apply H. }
    { simpl. intros [H | IND].
      - left. left. apply H.
      - apply IHl in IND. apply or_assoc. right. apply IND.
    }
  - induction l.
    { simpl. intros [F | T].
      - exfalso. apply F.
      - apply T.
    }
    { simpl. intros. apply or_assoc in H. inversion H.
      - left. apply H0.
      - right. apply IHl in H0. apply H0.
    }
Qed.

Fixpoint All {T : Type} (P : T -> Prop) (l : list T) : Prop :=
  match l with
  | [] => True
  | x::xs => P x /\ (All P xs)
  end.

Lemma All_In :
  forall T (P : T -> Prop) (l : list T),
    (forall x, In x l -> P x) <->
    All P l.
Proof.
  intros. split.
  - intros H. induction l.
    + simpl. auto.
    + simpl. simpl in H. split.
      * apply H. left. reflexivity.
      * apply IHl. intros. apply H. right. apply H0.
  - intros H. induction l.
    + simpl. intros. exfalso. apply H0.
    + simpl. intros. destruct H0.
      * inversion H. rewrite H0 in H1. apply H1.
      * inversion H. apply IHl. apply H2. apply H0.
Qed.

Definition combine_odd_even (Podd Peven : nat -> Prop) : nat -> Prop :=
  fun x => if (evenb x) then (Peven x) else (Podd x).
  
Theorem combine_odd_even_intro :
  forall(Podd Peven : nat -> Prop) (n : nat),
    (oddb n = true -> Podd n) ->
    (oddb n = false -> Peven n) ->
    combine_odd_even Podd Peven n.
Proof.
  intros Podd Peven n O E.
  unfold combine_odd_even.
  unfold oddb in O.
  unfold oddb in E.
  destruct (evenb n).
  - apply E. reflexivity.
  - apply O. reflexivity.
Qed.
  
Theorem combine_odd_even_elim_odd :
  forall(Podd Peven : nat -> Prop) (n : nat),
    combine_odd_even Podd Peven n ->
    oddb n = true ->
    Podd n.
Proof.
  intros.
  unfold oddb in H0.
  assert (NEG : negb (evenb n) = true -> evenb n = false).
  { intros. destruct (evenb n).
    - simpl in H1. rewrite H1. reflexivity.
    - reflexivity.
  }
  apply NEG in H0.
  unfold combine_odd_even in H.
  rewrite H0 in H.
  apply H.
Qed.

Theorem combine_odd_even_elim_even :
  forall(Podd Peven : nat -> Prop) (n : nat),
    combine_odd_even Podd Peven n ->
    oddb n = false ->
    Peven n.
Proof.
  intros.
  unfold oddb in H0.
  assert (NEG : negb (evenb n) = false -> evenb n = true).
  { intros. destruct (evenb n).
    - reflexivity.
    - simpl in H1. rewrite H1. reflexivity.
  }
  apply NEG in H0.
  unfold combine_odd_even in H.
  rewrite H0 in H.
  apply H.
Qed.

Example function_equality_ex1 : plus 3 = plus (pred 4).
Proof. reflexivity. Qed.

Axiom functional_extensionality : forall{X Y: Type}
                                   {f g : X -> Y},
    (forall(x:X), f x = g x) -> f = g.

Example function_equality_ex2 :
  (fun x => plus x 1) = (fun x => plus 1 x).
Proof.
  apply functional_extensionality. intros x.
  apply plus_comm.
Qed.

Print Assumptions function_equality_ex2.

Fixpoint rev_append {X} (l1 l2 : list X) : list X :=
  match l1 with
  | [] => l2
  | x :: l1' => rev_append l1' (x :: l2)
  end.

Definition tr_rev {X} (l : list X) : list X :=
  rev_append l [].

Lemma rev_append_app : forall X (l1 l2 : list X), rev_append l1 l2 = rev_append l1 [] ++ l2.
Proof.
  intros.
  generalize dependent l2.
  induction l1.
  - simpl. reflexivity.
  - simpl. intros. rewrite IHl1. symmetry.
    rewrite -> IHl1. rewrite <- app_assoc.
    simpl. reflexivity.
Qed.
  
Lemma tr_rev_correct : forall X, @tr_rev X = @rev X.
Proof.
  intros X.
  apply functional_extensionality.
  intros l.
  induction l.
  - unfold tr_rev. reflexivity.
  - simpl.
    unfold tr_rev. simpl.
    rewrite -> rev_append_app.
    rewrite <- IHl.
    unfold tr_rev.
    reflexivity.
Qed.

Theorem evenb_double : forall k, evenb (double k) = true.
Proof.
  intros k. induction k as [|k' IHk'].
  - reflexivity.
  - simpl. apply IHk'.
Qed.

Theorem evenb_double_conv : forall n,
    exists k, n = if evenb n then double k
             else S (double k).
Proof.
  intros.
  induction n.
  - simpl. exists 0. reflexivity.
  - rewrite evenb_S. inversion IHn.
    destruct (evenb n) eqn:EVN.
    + simpl. rewrite H. exists x. reflexivity.
    + simpl. rewrite H. exists (S x). reflexivity.
Qed.

Theorem even_bool_prop : forall n,
  evenb n = true <-> exists k, n = double k.
Proof.
  intros n. split.
  - intros H. destruct (evenb_double_conv n) as [k Hk].
    rewrite Hk. rewrite H. exists k. reflexivity.
  - intros [k Hk]. rewrite Hk. apply evenb_double.
Qed.

Theorem beq_nat_true_iff : forall n1 n2 : nat,
  beq_nat n1 n2 = true <-> n1 = n2.
Proof.
  intros n1 n2. split.
  - apply beq_nat_true.
  - intros H. rewrite H. rewrite <- beq_nat_refl. reflexivity.
Qed.

Lemma andb_true_iff : forall b1 b2:bool,
  b1 && b2 = true <-> b1 = true /\ b2 = true.
Proof.
  intros. split.
  - intros. destruct b1.
    + split.
      * reflexivity.
      * destruct b2.
        { reflexivity. }
        { simpl in H. apply H. }
    + split.
      * simpl in H. apply H.
      * destruct b2.
        { reflexivity. }
        { simpl in H. apply H. }
  - intros [L R]. rewrite L. rewrite R. reflexivity.
Qed.
  
Lemma orb_true_iff : forall b1 b2,
  b1 || b2 = true <-> b1 = true \/ b2 = true.
Proof.
  intros. split.
  - intros. destruct b1.
    + left. reflexivity.
    + destruct b2.
      * right. reflexivity.
      * simpl in H. left. apply H.
  - intros [L | R].
    + rewrite L. reflexivity.
    + rewrite R. destruct b1.
      * simpl. reflexivity.
      * simpl. reflexivity.
Qed.

Theorem beq_nat_false_iff : forall x y : nat,
  beq_nat x y = false <-> x <> y.
Proof.
  intros. split.
  - intros. destruct (beq_nat x y) eqn:BQ.
    + inversion H.
    + unfold not. intros. rewrite H0 in BQ.
      rewrite <- beq_nat_refl in BQ.
      inversion BQ.
  - intros. unfold not in H. induction x as [| x' ].
    + induction y as [| y' ].
      * simpl. exfalso. apply H. reflexivity.
      * simpl. reflexivity.
    + induction y as [| y' ].
      * simpl. reflexivity.
      * simpl. destruct (beq_nat x' y') eqn:BQ.
        { exfalso. apply H. apply f_equal. apply beq_nat_true_iff. apply BQ. }
        { reflexivity. }
Qed.

Fixpoint beq_list {A : Type} (beq : A -> A -> bool)
         (l1 l2 : list A) : bool :=
  match (l1, l2) with
  | ([], []) => true
  | (x::xs, y::ys) => if (beq x y) then (beq_list beq xs ys) else false
  | (_, _) => false
  end.
                     
Lemma beq_list_true_iff :
  forall A (beq : A -> A -> bool),
    (forall a1 a2, beq a1 a2 = true <-> a1 = a2) ->
    forall l1 l2, beq_list beq l1 l2 = true <-> l1 = l2.
Proof.
  intros. split.
  - generalize dependent l2. induction l1 as [| x1 xs1 IH1 ].
    + induction l2 as [| x2 xs2 IH2 ].
      { simpl. intros. reflexivity. }
      { simpl. intros. inversion H0. }
    + induction l2 as [| x2 xs2 IH2 ].
      { simpl. intros. inversion H0. }
      { simpl. intros. destruct (beq x1 x2) eqn:BEQ. 
        - apply H in BEQ. rewrite BEQ.
          assert (EQ: xs1 = xs2 -> x2 :: xs1 = x2 :: xs2).
          { intros. rewrite H1. reflexivity. }
          apply EQ. apply IH1. apply H0.
        - inversion H0.
      }
  - generalize dependent l2. induction l1 as [| x1 xs1 IH1 ].
    + induction l2 as [| x2 xs2 IH2 ].
      * simpl. reflexivity.
      * simpl. intros. inversion H0.
    + induction l2 as [| x2 xs2 IH2 ].
      * intros. rewrite H0. simpl. reflexivity.
      * intros. inversion H0. rewrite <- H3.
        simpl. assert (T : beq x2 x2 = true).
        { inversion H2. rewrite H2 in H1. apply H in H1. apply H1. }
        rewrite T. apply IH1. reflexivity.
Qed.

Fixpoint forallb {X : Type} (test : X -> bool) (l : list X) : bool :=
  match l with
  | [] => true
  | x :: l' => andb (test x) (forallb test l')
  end.

Theorem forallb_true_iff : forall X test (l : list X),
   forallb test l = true <-> All (fun x => test x = true) l.
Proof.
  intros. split.
  - intros. induction l as [| x xs IH ].
    + simpl. simpl in H. auto.
    + simpl. inversion H. split.
      * assert (TX: test x && forallb test xs = true ->
                    test x = true).
        { destruct (test x) eqn:T.
          - simpl. reflexivity.
          - simpl. auto.
        }
        inversion H1. apply TX in H2. rewrite H1. rewrite H2.
        reflexivity.
      * rewrite H1. apply IH.
        apply andb_true_elim2 in H1. apply H1.
  - intros. induction l as [| x xs IH ].
    + simpl. reflexivity.
    + simpl. simpl in H. inversion H.
      rewrite H0. simpl. apply IH.
      apply H1.
Qed.

Definition excluded_middle := forall P : Prop,
    P \/ ~ P.

Theorem restricted_excluded_middle : forall P b,
  (P <-> b = true) -> P \/ ~ P.
Proof.
  intros P [] H.
  - left. rewrite H. reflexivity.
  - right. rewrite H. intros contra. inversion contra.
Qed.

Theorem restricted_excluded_middle_eq : forall(n m : nat),
    n = m \/ n <> m.
Proof.
  intros n m.
  apply (restricted_excluded_middle (n = m) (beq_nat n m)).
  symmetry.
  apply beq_nat_true_iff.
Qed.

Theorem excluded_middle_irrefutable: forall(P:Prop),
    ~ ~ (P \/ ~ P).
Proof.
  intros.
  unfold not.
  intros. apply H.
  right. intros. apply H. left. apply H0.
Qed.

Theorem not_exists_dist :
  excluded_middle ->
  forall (X:Type) (P : X -> Prop),
    ~ (exists x, ~ P x) -> (forall x, P x).
Proof.
  unfold excluded_middle.
  intros EX X P H x.
  unfold not in H. unfold not in EX.
  destruct (EX (P x)) eqn:EXCL.
  - apply p.
  - exfalso. apply H. exists x. apply f.
Qed.

Definition peirce := forall P Q: Prop,
    ((P -> Q) -> P) -> P.

Definition double_negation_elimination := forall P:Prop,
    ~~P -> P.

Definition de_morgan_not_and_not := forall P Q:Prop,
    ~(~P /\ ~Q) -> P \/ Q.

Definition implies_to_or := forall P Q:Prop,
    (P -> Q) -> (~ P \/ Q).

(* TODO: proof equivalence *)