Require Export Induction.

Module NatList.

Inductive natprod : Type :=
| pair : nat -> nat -> natprod.

Definition fst (p : natprod) : nat :=
  match p with
  | pair x y => x
  end.

Definition snd (p : natprod) : nat :=
  match p with
  | pair x y => y
  end.

Compute (fst (pair 3 5)).

Notation "( x , y )" := (pair x y).

Compute (snd (2,3)).

Definition swap_pair (p : natprod) : natprod :=
  match p with
  | (x,y) => (y,x)
  end.

Theorem surjective_pairing' : forall(n m : nat),
  (n,m) = (fst (n,m), snd (n,m)).
Proof.
  reflexivity. Qed.

Theorem surjective_pairing : forall(p : natprod),
  p = (fst p, snd p).
Proof.
  intros p. destruct p as [n m]. simpl. reflexivity. Qed.

Theorem snd_fst_is_swap : forall(p : natprod),
  (snd p, fst p) = swap_pair p.
Proof.
  intros.
  destruct p as [n m].
  simpl. reflexivity.
Qed.
  
Theorem fst_swap_is_snd : forall(p : natprod),
  fst (swap_pair p) = snd p.
Proof.
  intros. destruct p as [n m]. reflexivity.
Qed.

Inductive natlist : Type :=
| nil : natlist
| cons : nat -> natlist -> natlist.

Notation "x :: l" := (cons x l)
                     (at level 60, right associativity).
Notation "[ ]" := nil.
Notation "[ x ; .. ; y ]" := (cons x .. (cons y nil) ..).

Definition mylist := 1::2::[].

Fixpoint repeat (n count : nat) : natlist :=
  match count with
  | O => nil
  | S count' => n :: (repeat n count')
  end.

Fixpoint length (lst : natlist) : nat :=
  match lst with
  | nil => O
  | h :: tl => S (length tl)
  end.

Fixpoint app (l1 l2 : natlist) : natlist :=
  match l1 with
  | nil => l2
  | h :: tl => h :: (app tl l2)
  end.

Notation "l ++ r" := (app l r)
                       (right associativity, at level 60).

Compute ([1;2;3] ++ [4;5]).

Definition hd (default:nat) (l:natlist) : nat :=
  match l with
  | nil => default
  | h :: t => h
  end.

Definition tl (l:natlist) : natlist :=
  match l with
  | nil => nil
  | h :: t => t
  end.

Example test_hd1: hd 0 [1;2;3] = 1.
Proof. reflexivity. Qed.
Example test_hd2: hd 0 [] = 0.
Proof. reflexivity. Qed.
Example test_tl: tl [1;2;3] = [2;3].
Proof. reflexivity. Qed.

Fixpoint nonzeros (l:natlist) : natlist :=
  match l with
  | nil => nil
  | O :: tl => nonzeros tl
  | h :: tl => h :: (nonzeros tl)
  end.

Example test_nonzeros:
  nonzeros [0;1;0;2;3;0;0] = [1;2;3].
reflexivity. Qed.

Fixpoint oddmembers (l:natlist) : natlist :=
  match l with
  | nil => nil
  | h :: tl =>
    match (oddb h) with
    | true => h :: (oddmembers tl)
    | false => oddmembers tl
    end
  end.

Example test_oddmembers:
  oddmembers [0;1;0;2;3;0;0] = [1;3].
reflexivity. Qed.

Definition countoddmembers (l:natlist) : nat :=
  length (oddmembers l).
  
Example test_countoddmembers1:
  countoddmembers [1;0;3;1;4;5] = 4.
reflexivity. Qed.

Example test_countoddmembers2:
  countoddmembers [0;2;4] = 0.
reflexivity. Qed.

Example test_countoddmembers3:
  countoddmembers nil = 0.
reflexivity. Qed.

Fixpoint alternate (l1 l2 : natlist) : natlist :=
  match l1 with
  | nil => l2
  | h1::tl1 =>
    match l2 with
    | nil => l1
    | h2::tl2 => h1::h2::(alternate tl1 tl2)
    end
  end.
    
Example test_alternate1:
  alternate [1;2;3] [4;5;6] = [1;4;2;5;3;6].
reflexivity. Qed.

Example test_alternate2:
  alternate [1] [4;5;6] = [1;4;5;6].
reflexivity. Qed.

Example test_alternate3:
  alternate [1;2;3] [4] = [1;4;2;3].
reflexivity. Qed.

Example test_alternate4:
  alternate [] [20;30] = [20;30].
reflexivity. Qed.

Definition bag := natlist.

Fixpoint count (v:nat) (s:bag) : nat :=
  match s with
  | nil => O
  | h::tl =>
    match (beq_nat h v) with
    | true => S (count v tl)
    | false => count v tl
    end
  end.

Example test_count1: count 1 [1;2;3;1;4;1] = 3.
reflexivity. Qed.
Example test_count2: count 6 [1;2;3;1;4;1] = 0.
reflexivity. Qed.


Definition sum : bag -> bag -> bag := app.

Example test_sum1: count 1 (sum [1;2;3] [1;4;1]) = 3.
reflexivity. Qed.

Definition add (v:nat) (s:bag) : bag := v::s.

Example test_add1: count 1 (add 1 [1;4;1]) = 3.
reflexivity. Qed.
Example test_add2: count 5 (add 1 [1;4;1]) = 0.
reflexivity. Qed.

Definition member (v:nat) (s:bag) : bool := leb 1 (count v s).
                                    
Example test_member1: member 1 [1;4;1] = true.
reflexivity. Qed.
Example test_member2: member 2 [1;4;1] = false.
reflexivity. Qed.

Fixpoint remove_one (v:nat) (s:bag) : bag :=
  match s with
  | nil => nil
  | h::tl =>
    match (beq_nat h v) with
    | true  => tl
    | false => h::(remove_one v tl)
    end
  end.

Example test_remove_one1:
  count 5 (remove_one 5 [2;1;5;4;1]) = 0.
reflexivity. Qed.

Example test_remove_one2:
  count 5 (remove_one 5 [2;1;4;1]) = 0.
reflexivity. Qed.

Example test_remove_one3:
  count 4 (remove_one 5 [2;1;4;5;1;4]) = 2.
reflexivity. Qed.

Example test_remove_one4:
  count 5 (remove_one 5 [2;1;5;4;5;1;4]) = 1.
reflexivity. Qed.

Fixpoint remove_all (v:nat) (s:bag) : bag :=
  match s with
  | nil => nil
  | h::tl =>
    match (beq_nat h v) with
    | true  => (remove_all v tl)
    | false => h::(remove_all v tl)
    end
  end.

Example test_remove_all1: count 5 (remove_all 5 [2;1;5;4;1]) = 0.
reflexivity. Qed.
Example test_remove_all2: count 5 (remove_all 5 [2;1;4;1]) = 0.
reflexivity. Qed.
Example test_remove_all3: count 4 (remove_all 5 [2;1;4;5;1;4]) = 2.
reflexivity. Qed.
Example test_remove_all4: count 5 (remove_all 5 [2;1;5;4;5;1;4;5;1;4]) = 0.
reflexivity. Qed.

Fixpoint subset (s1:bag) (s2:bag) : bool :=
  match s1 with
  | nil     => true
  | h1::tl1 =>
    match (member h1 s2) with
    | true  => subset tl1 (remove_one h1 s2)
    | false => false
    end
  end.
                                      
Example test_subset1: subset [1;2] [2;1;4;1] = true.
reflexivity. Qed.
Example test_subset2: subset [1;2;2] [2;1;4;1] = false.
reflexivity. Qed.

Theorem nil_app : forall l:natlist,
    [] ++ l = l.
Proof. reflexivity. Qed.

Theorem tl_length_pred : forall l:natlist,
  pred (length l) = length (tl l).
Proof.
  intros l. destruct l as [| n l'].
  - (* l = nil *)
    reflexivity.
  - (* l = cons n l' *)
    reflexivity. Qed.

Theorem app_assoc : forall l1 l2 l3 : natlist,
  (l1 ++ l2) ++ l3 = l1 ++ (l2 ++ l3).
Proof.
  intros l1 l2 l3. induction l1 as [| n l1' IHl1'].
  - (* l1 = nil *)
    reflexivity.
  - (* l1 = cons n l1' *)
    simpl. rewrite -> IHl1'. reflexivity. Qed.

Fixpoint rev (l:natlist) : natlist :=
  match l with
  | nil => nil
  | h :: t => rev t ++ [h]
  end.

Example test_rev1: rev [1;2;3] = [3;2;1].
Proof. reflexivity. Qed.
Example test_rev2: rev nil = nil.
Proof. reflexivity. Qed.

Theorem app_length : forall l1 l2 : natlist,
  length (l1 ++ l2) = (length l1) + (length l2).
Proof.
  (* WORKED IN CLASS *)
  intros l1 l2. induction l1 as [| n l1' IHl1'].
  - (* l1 = nil *)
    reflexivity.
  - (* l1 = cons *)
    simpl. rewrite -> IHl1'. reflexivity. Qed.

Theorem rev_length : forall l : natlist,
  length (rev l) = length l.
Proof.
  intros. induction l as [| n l' IH ].
  (* l = nil *)
  - reflexivity.
  (* l = n::l' *)
  - simpl.
    rewrite app_length, plus_comm.
    rewrite IH.
    reflexivity.
Qed.


    
Theorem app_nil_r : forall l : natlist,
  l ++ [] = l.
Proof.
  intros.
  induction l as [| n l' IH].
  (* l = [ ] *)
  - reflexivity.
  (* l = n::l' *)
  - simpl. rewrite IH. reflexivity.
Qed.
  
Theorem rev_app_distr: forall l1 l2 : natlist,
  rev (l1 ++ l2) = rev l2 ++ rev l1.
Proof.
  intros.
  induction l1 as [| n1 l'1 IH ].
  - simpl. rewrite app_nil_r. reflexivity.
  - simpl. rewrite IH, app_assoc. reflexivity.
Qed.
  
Theorem rev_involutive : forall l : natlist,
  rev (rev l) = l.
Proof.
  intros.
  induction l as [| n l' IH ].
  - reflexivity.
  - simpl. rewrite rev_app_distr. simpl. rewrite IH. reflexivity.
Qed.

Theorem app_assoc4 : forall l1 l2 l3 l4 : natlist,
  l1 ++ (l2 ++ (l3 ++ l4)) = ((l1 ++ l2) ++ l3) ++ l4.
Proof.
  intros.
  induction l1 as [| n1 l'1 IH ].
  - simpl. rewrite app_assoc. reflexivity.
  - simpl. rewrite IH. reflexivity.
Qed.

Lemma nonzeros_app : forall l1 l2 : natlist,
  nonzeros (l1 ++ l2) = (nonzeros l1) ++ (nonzeros l2).
Proof.
  intros.
  induction l1 as [| n1 l'1 IH ].
  - reflexivity.
  - destruct n1.
    + simpl. rewrite IH. reflexivity.
    + simpl. rewrite IH. reflexivity.
Qed.

Inductive nlprod : Type :=
| nlpair : natlist -> natlist -> nlprod.

Fixpoint beq_natlist (l1 l2 : natlist) : bool :=
  match (nlpair l1 l2) with
  | (nlpair nil nil) => true
  | (nlpair (h1::tl1) (h2::tl2)) => (beq_nat h1 h2) && (beq_natlist tl1 tl2)
  | _ => false
  end.

Example test_beq_natlist1 :
  (beq_natlist nil nil = true).
reflexivity. Qed.
Example test_beq_natlist2 :
  beq_natlist [1;2;3] [1;2;3] = true.
reflexivity. Qed.
Example test_beq_natlist3 :
  beq_natlist [1;2;3] [1;2;4] = false.
reflexivity. Qed.

Lemma beq_nat_n_n : forall n : nat, beq_nat n n = true.
Proof.
  intros.
  induction n as [| n' IH ].
  - simpl. reflexivity.
  - simpl. rewrite IH. reflexivity.
Qed.

Theorem beq_natlist_refl : forall l:natlist,
  true = beq_natlist l l.
Proof.
  intros.
  induction l as [| n l IH ].
  - reflexivity.
  - simpl. rewrite beq_nat_n_n. simpl. rewrite <- IH. reflexivity.
Qed.

Theorem count_member_nonzero : forall(s : bag),
  leb 1 (count 1 (1 :: s)) = true.
Proof.
Admitted.

Theorem ble_n_Sn : forall n,
  leb n (S n) = true.
Proof.
  intros n. induction n as [| n' IHn'].
  - (* 0 *)
    simpl. reflexivity.
  - (* S n' *)
    simpl. rewrite IHn'. reflexivity. Qed.

Theorem remove_decreases_count: forall (s : bag),
  leb (count 0 (remove_one 0 s)) (count 0 s) = true.
Proof.
  intros.
  induction s as [| n s' IH ].
  - reflexivity.
  - destruct n.
    + simpl. rewrite ble_n_Sn. reflexivity.
    + simpl. rewrite IH. reflexivity.
Qed.

(* TODO ex *)

Inductive natoption : Type :=
| Some : nat -> natoption
| None : natoption.

Fixpoint nth_error (l:natlist) (n:nat) : natoption :=
  match l with
  | nil => None
  | a :: l' => match beq_nat n O with
              | true => Some a
              | false => nth_error l' (pred n)
              end
  end.

Example test_nth_error1 : nth_error [4;5;6;7] 0 = Some 4.
Proof. reflexivity. Qed.
Example test_nth_error2 : nth_error [4;5;6;7] 3 = Some 7.
Proof. reflexivity. Qed.
Example test_nth_error3 : nth_error [4;5;6;7] 9 = None.
Proof. reflexivity. Qed.

Fixpoint nth_error' (l:natlist) (n:nat) : natoption :=
  match l with
  | nil => None
  | a :: l' => if beq_nat n O then Some a
              else nth_error' l' (pred n)
  end.

Definition option_elim (d : nat) (o : natoption) : nat :=
  match o with
  | Some n' => n'
  | None => d
  end.

Definition hd_error (l : natlist) : natoption :=
  match l with
  | nil => None
  | h::tl => Some h
  end.
                                      
Example test_hd_error1 : hd_error [] = None.
Proof. reflexivity. Qed.
Example test_hd_error2 : hd_error [1] = Some 1.
Proof. reflexivity. Qed.
Example test_hd_error3 : hd_error [5;6] = Some 5.
Proof. reflexivity. Qed.

Theorem option_elim_hd : forall (l:natlist) (default:nat),
  hd default l = option_elim default (hd_error l).
Proof.
  intros.
  induction l as [| n l' IH ].
  - simpl. reflexivity.
  - simpl. reflexivity.
Qed.

End NatList.

Module PartialMap.
Export NatList.

Inductive id : Type :=
| Id : nat -> id.

Definition beq_id (x1 x2 : id) :=
  match x1, x2 with
  | Id n1, Id n2 => beq_nat n1 n2
  end.

Theorem beq_id_refl : forall x, true = beq_id x x.
Proof.
  intros. destruct x.
  - simpl. rewrite beq_nat_n_n. reflexivity.
Qed.

Inductive partial_map : Type :=
  | empty : partial_map
  | record : id -> nat -> partial_map -> partial_map.

Definition update (d : partial_map)
                  (x : id) (value : nat)
                  : partial_map :=
  record x value d.

Fixpoint find (x : id) (d : partial_map) : natoption :=
  match d with
  | empty => None
  | record y v d' => if beq_id x y
                    then Some v
                    else find x d'
  end.

(* TODO exs *)


End PartialMap.