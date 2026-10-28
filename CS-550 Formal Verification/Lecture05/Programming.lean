inductive MyList (A : Type): Type where
  | nil : MyList A
  | cons (head : A) (tail : MyList A) : MyList A

open MyList

@[grind]
def len {A : Type} (lst: MyList A): Nat :=
  match lst with
  | .nil => (0 : Nat)
  | .cons _ tl => 1 + len tl

def append {A : Type} (xs ys : MyList A) : MyList A :=
  match xs with
  | .nil => ys
  | .cons h tl => cons h (append tl ys)

def exampleList : MyList Int := cons 1 (cons 2 nil)
#eval len exampleList
#eval len (append exampleList (cons (3 : Nat) nil))

theorem appendMorph {A: Type} (xs ys : MyList A):  -- holds for arbitrary A,xs,ys
  len (append xs ys) = len xs + len ys := by
  match xs with
  | .nil =>
    rw[append]
    rw[len]
    simp
  | .cons h t =>
    simp[len, append]
    let IH : len (append t ys) = len t + len ys := appendMorph t ys
    rw[IH]
    rw[Nat.add_assoc]

def compose {A B C : Type} (f : A → B) (g : B → C) : A → C :=
  fun (a : A) =>
    let b := f a
    g b

@[grind]
def map {A B : Type} (f : A → B) (lst: MyList A) : MyList B :=
  match lst with
  | .nil => nil
  | .cons h t => cons (f h) (map f t)

#eval map (fun x => x + 1) exampleList

theorem mapKeepsLength {A B : Type} (as : MyList A) (f : A → B) :
    len as = len (map f as) := by
    induction as generalizing f with
    | nil => simp[map, len]
    | cons h t ih =>
      simp[len, map]
      rw[ih]

theorem mapComp {A B C : Type} (f : A → B) (g : B → C) (lst : MyList A) :
    (map g (map f lst)) = map (fun a => g (f a)) lst := by
  induction lst generalizing f g with
  | nil => grind[map]
  | cons h t ih =>
    simp[map]
    rw[ih]
