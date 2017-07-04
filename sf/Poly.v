Require Export Lists.

Inductive list (X:Type) : Type :=
| nil : list X
| cons : X -> list X -> list X.

Fixpoint repeat (X : Type) (x : X) (count : nat) : list X :=
  match count with
  | 0 => nil X
  | S count' => cons X x (repeat X x count')
  end.

Example test_repeat1 :
  repeat nat 4 2 = cons nat 4 (cons nat 4 (nil nat)).
Proof. reflexivity. Qed.

Example test_repeat2 :
  repeat bool false 1 = cons bool false (nil bool).
Proof. reflexivity. Qed.

Fixpoint repeat' X x count : list X :=
  match count with
  | 0 => nil X
  | S count' => cons X x (repeat' X x count')
  end.

Check repeat'.

Arguments nil {X}.
Arguments cons {X} _ _.
Arguments repeat {X} x count.

Check repeat.

Fixpoint app {X : Type} (l1 l2 : list X)
             : (list X) :=
  match l1 with
  | nil => l2
  | cons h t => cons h (app t l2)
  end.

Fixpoint rev {X:Type} (l:list X) : list X :=
  match l with
  | nil => nil
  | cons h t => app (rev t) (cons h nil)
  end.

Fixpoint length {X : Type} (l : list X) : nat :=
  match l with
  | nil => 0
  | cons _ l' => S (length l')
  end.

Example test_rev1 :
  rev (cons 1 (cons 2 nil)) = (cons 2 (cons 1 nil)).
Proof. reflexivity. Qed.

Example test_rev2:
  rev (cons true nil) = cons true nil.
Proof. reflexivity. Qed.

Example test_length1: length (cons 1 (cons 2 (cons 3 nil))) = 3.
Proof. reflexivity. Qed.

Notation "x :: y" := (cons x y)
                     (at level 60, right associativity).
Notation "[ ]" := nil.
Notation "[ x ; .. ; y ]" := (cons x .. (cons y []) ..).
Notation "x ++ y" := (app x y)
                     (at level 60, right associativity).

Theorem app_nil_r : forall(X:Type), forall l:list X,
  l ++ [] = l.
Proof.
  intros.
  induction l as [| n l' IH ].
  - reflexivity.
  - simpl. rewrite IH. reflexivity.
Qed.
  
Theorem app_assoc : forall A (l m n:list A),
  l ++ m ++ n = (l ++ m) ++ n.
Proof.
  intros.
  induction l as [| l l' IH ].
  - simpl. reflexivity.
  - simpl. rewrite IH. reflexivity.
Qed.
  
Lemma app_length : forall (X:Type) (l1 l2 : list X),
  length (l1 ++ l2) = length l1 + length l2.
Proof.
  intros.
  induction l1 as [| n1 l1' IH1 ].
  - reflexivity.
  - simpl. rewrite IH1. reflexivity.
Qed.

Theorem rev_app_distr: forall X (l1 l2 : list X),
  rev (l1 ++ l2) = rev l2 ++ rev l1.
Proof.
  intros.
  induction l1 as [| n1 l1' IH1 ].
  - simpl. rewrite app_nil_r. reflexivity.
  - simpl. rewrite IH1, app_assoc. reflexivity.
Qed.
  
Theorem rev_involutive : forall X : Type, forall l : list X,
  rev (rev l) = l.
Proof.
  intros.
  induction l as [| n l' IH ].
  - reflexivity.
  - simpl. rewrite rev_app_distr. simpl. rewrite IH. reflexivity.
Qed.

Inductive prod (X Y : Type) : Type :=
| pair : X -> Y -> prod X Y.

Arguments pair {X} {Y} _ _.

Notation "( x , y )" := (pair x y).

Notation "X * Y" := (prod X Y) : type_scope.

Definition fst {X Y : Type} (p : X * Y) : X :=
  match p with
  | (x, y) => x
  end.

Definition snd {X Y : Type} (p : X * Y) : Y :=
  match p with
  | (x, y) => y
  end.

Fixpoint combine {X Y : Type} (lx : list X) (ly : list Y)
           : list (X*Y) :=
  match lx, ly with
  | [], _ => []
  | _, [] => []
  | x :: tx, y :: ty => (x, y) :: (combine tx ty)
  end.

Check @combine.

Fixpoint split {X Y : Type} (l : list (X*Y))
  : (list X) * (list Y) :=
  match l with
  | nil => ([],[])
  | (x,y)::tl =>
    match split tl with
    | (tlx, tly) => (x::tlx,y::tly)
    end
  end.

Example test_split:
  split [(1,false);(2,false)] = ([1;2],[false;false]).
Proof.
  reflexivity. Qed.

Inductive option (X:Type) : Type :=
  | Some : X -> option X
  | None : option X.

Arguments Some {X} _.
Arguments None {X}.

Fixpoint nth_error {X : Type} (l : list X) (n : nat)
                   : option X :=
  match l with
  | [] => None
  | a :: l' => if beq_nat n O then Some a else nth_error l' (pred n)
  end.

Example test_nth_error1 : nth_error [4;5;6;7] 0 = Some 4.
Proof. reflexivity. Qed.
Example test_nth_error2 : nth_error [[1];[2]] 1 = Some [2].
Proof. reflexivity. Qed.
Example test_nth_error3 : nth_error [true] 2 = None.
Proof. reflexivity. Qed.

Definition hd_error {X : Type} (l : list X) : option X :=
  match l with
  | nil => None
  | h::tl => Some h
  end.

Check @hd_error.

Example test_hd_error1 : hd_error [1;2] = Some 1.
Proof. reflexivity. Qed.
Example test_hd_error2 : hd_error [[1];[2]] = Some [1].
Proof. reflexivity. Qed.

Fixpoint filter {X:Type} (test: X -> bool) (l:list X)
  : (list X) :=
  match l with
  | [] => []
  | h::tl => if test h
            then h :: (filter test tl)
            else filter test tl
  end.

Example test_filter1: filter evenb [1;2;3;4] = [2;4].
Proof. reflexivity. Qed.

Definition length_is_1 {X : Type} (l : list X) : bool :=
  beq_nat (length l) 1.

Example test_filter2:
    filter length_is_1
           [ [1; 2]; [3]; [4]; [5;6;7]; []; [8] ]
  = [ [3]; [4]; [8] ].
Proof. reflexivity. Qed.


Definition countoddmembers' (l:list nat) : nat :=
  length (filter oddb l).

Example test_countoddmembers'1: countoddmembers' [1;0;3;1;4;5] = 4.
Proof. reflexivity. Qed.
Example test_countoddmembers'2: countoddmembers' [0;2;4] = 0.
Proof. reflexivity. Qed.
Example test_countoddmembers'3: countoddmembers' nil = 0.
Proof. reflexivity. Qed.

Example test_filter2':
    filter (fun l => beq_nat (length l) 1)
           [ [1; 2]; [3]; [4]; [5;6;7]; []; [8] ]
  = [ [3]; [4]; [8] ].
Proof. reflexivity. Qed.

Definition filter_even_gt7 (l : list nat) : list nat :=
  filter (fun n => (blt_nat 7 n) && (evenb n)) l.
                                               
Example test_filter_even_gt7_1 :
  filter_even_gt7 [1;2;6;9;10;3;12;8] = [10;12;8].
Proof. reflexivity. Qed.

Example test_filter_even_gt7_2 :
  filter_even_gt7 [5;2;6;19;129] = [].
Proof. reflexivity. Qed.

Definition partition {X : Type}
           (test : X -> bool)
           (l : list X)
  : list X * list X :=
  (filter test l,
   filter (fun n => negb (test n)) l).
                                   
Example test_partition1: partition oddb [1;2;3;4;5] = ([1;3;5], [2;4]).
Proof. reflexivity. Qed.
Example test_partition2: partition (fun x => false) [5;9;0] = ([], [5;9;0]).
Proof. reflexivity. Qed.

Fixpoint map {X Y:Type} (f:X -> Y) (l:list X) : (list Y) :=
  match l with
  | [] => []
  | h :: t => (f h) :: (map f t)
  end.

Example test_map1: map (fun x => plus 3 x) [2;0;2] = [5;3;5].
Proof. reflexivity. Qed.
Example test_map2:
  map oddb [2;1;2;5] = [false;true;false;true].
Proof. reflexivity. Qed.

Lemma map_app_distr : forall(X Y : Type) (f: X -> Y) (l1 l2: list X),
    map f (l1 ++ l2) = (map f l1) ++ (map f l2).
Proof.
  intros. induction l1 as [| n1 l1' IH1].
  - reflexivity.
  - simpl. rewrite IH1. reflexivity.
Qed.

Theorem map_rev : forall(X Y : Type) (f : X -> Y) (l : list X),
  map f (rev l) = rev (map f l).
Proof.
  intros. induction l as [| n l' IH ].
  - reflexivity.
  - simpl. rewrite map_app_distr. simpl. rewrite IH. reflexivity.
Qed.

Fixpoint flat_map {X Y:Type} (f:X -> list Y) (l:list X)
  : (list Y) :=
  match l with
  | [] => []
  | h::tl => (f h) ++ (flat_map f tl)
  end.

Example test_flat_map1:
  flat_map (fun n => [n;n;n]) [1;5;4]
  = [1; 1; 1; 5; 5; 5; 4; 4; 4].
Proof. reflexivity. Qed.

Definition option_map {X Y : Type} (f : X -> Y) (xo : option X)
                      : option Y :=
  match xo with
    | None => None
    | Some x => Some (f x)
  end.

Fixpoint fold {X Y:Type} (f: X->Y->Y) (l:list X) (b:Y)
                         : Y :=
  match l with
  | nil => b
  | h :: t => f h (fold f t b)
  end.

Module Exercises.

  Definition fold_length {X : Type} (l : list X) : nat :=
  fold (fun _ n => S n) l 0.

  Example test_fold_length1 : fold_length [4;7;0] = 3.
  Proof. reflexivity. Qed.

  Theorem fold_length_correct : forall X (l : list X),
      fold_length l = length l.
  Proof.
    intros. induction l as [| n l' IH].
    - reflexivity.
    - simpl. rewrite <- IH. reflexivity.
  Qed.
  
  Definition fold_map {X Y:Type} (f : X -> Y) (l : list X)
    : list Y :=
    fold (fun x acc => [f x] ++ acc) l [].

  Theorem fold_map_correct : forall X Y (f : X -> Y) (l : list X),
      fold_map f l = map f l.
  Proof.
    intros. induction l as [| n l' IH].
    - reflexivity.
    - simpl. rewrite <- IH. reflexivity.
  Qed.

  Definition prod_curry {X Y Z : Type}
             (f : X * Y -> Z) (x : X) (y : Y) : Z := f (x, y).

  Definition prod_uncurry {X Y Z : Type}
             (f : X -> Y -> Z) (p : X * Y) : Z := f (fst p) (snd p).

  Example test_map2: map (fun x => plus 3 x) [2;0;2] = [5;3;5].
  Proof. reflexivity. Qed.

  Check @prod_curry.
  Check @prod_uncurry.

  Theorem uncurry_curry : forall(X Y Z : Type)
                           (f : X -> Y -> Z)
                           x y,
      prod_curry (prod_uncurry f) x y = f x y.
  Proof.
    intros. reflexivity.
  Qed.

  Theorem curry_uncurry : forall(X Y Z : Type)
                           (f : (X * Y) -> Z) (p : X * Y),
      prod_uncurry (prod_curry f) p = f p.
  Proof.
    intros. destruct p. reflexivity.
  Qed.

  Fixpoint nth_error {X : Type} (l : list X) (n : nat) : option X :=
     match l with
     | [] => None
     | a :: l' => if beq_nat n O then Some a else nth_error l' (pred n)
     end.

  Module Church.
    Definition nat := forall X : Type, (X -> X) -> X -> X.
    Definition zero : nat := fun (X : Type) (f : X -> X) (x : X) => x.
    Definition one : nat := fun (X : Type) (f : X -> X) (x : X) => f x.
    Definition two : nat := fun (X : Type) (f : X -> X) (x : X) => f (f x).
    Definition three : nat := fun (X : Type) (f : X -> X) (x : X) => f (f (f x)).
    
    Definition succ (n : nat) : nat := fun (X : Type) (f : X -> X) (x : X) => f (n X f x).
    Example succ_1 : succ zero = one.
    Proof. reflexivity. Qed.
    Example succ_2 : succ one = two.
    Proof. reflexivity. Qed.
    Example succ_3 : succ two = three.
    Proof. reflexivity. Qed.

    Definition plus (n m : nat) : nat := fun (X : Type) (f : X -> X) (x : X) => n X f (m X f x).
    Example plus_1 : plus zero one = one.
    Proof. reflexivity. Qed.
    Example plus_2 : plus two three = plus three two.
    Proof. reflexivity. Qed.
    Example plus_3 : plus (plus two two) three = plus one (plus three three).
    Proof. reflexivity. Qed.

    Definition mult (n m : nat) : nat := fun (X : Type) (f : X -> X) => n X (m X f).
    Example mult_1 : mult one one = one.
    Proof. reflexivity. Qed.
    Example mult_2 : mult zero (plus three three) = zero.
    Proof. reflexivity. Qed.
    Example mult_3 : mult two three = plus three three.
    Proof. reflexivity. Qed.

    Definition exp (n m : nat) : nat := fun (X : Type) => m (X -> X) (n X).
    Example exp_1 : exp two two = plus two two.
    Proof. reflexivity. Qed.
    Example exp_2 : exp three two = plus (mult two (mult two two)) one.
    Proof. reflexivity. Qed.
    Example exp_3 : exp three zero = one.
    Proof. reflexivity. Qed.

  End Church.

End Exercises.