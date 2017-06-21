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

