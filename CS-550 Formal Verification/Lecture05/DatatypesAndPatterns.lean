/-
  Datatypes that allow choices are called sum types
  and datatypes that can include instances of themselves are called recursive datatypes.
  Recursive sum types are called inductive datatypes,
  because mathematical induction may be used to prove statements about them.
  When programming, inductive datatypes are consumed through pattern matching and recursive functions.
-/

inductive Boolean where
| false : Boolean
| true : Boolean

inductive Natural where
| zero : Natural
| succ (n : Natural) : Natural


#eval Natural.zero
#eval Natural.succ (Natural.zero)

-- Defining an inductive type also creates a namespace containing its constructors.
open Natural
#eval zero
#eval succ (zero)


def isZeroNatural (n : Natural) : Boolean :=
  match n with
|   Natural.zero => Boolean.true
|   Natural.succ _ => Boolean.false

def isZero (n : Nat) : Bool :=
  match n with
  | Nat.zero => true
  | Nat.succ _ => false

#eval isZero 0
#eval isZero 4

def pred (n : Nat) : Nat :=
  match n with
  | Nat.zero => Nat.zero
  | Nat.succ k => k

#eval pred 0
#eval pred 101


def even (n : Nat) : Bool :=
  match n with
  | Nat.zero => true
  | Nat.succ k => not (even k)

def plus (n : Nat) (k : Nat) : Nat :=
  match k with
  | Nat.zero => n
  | Nat.succ k' => Nat.succ (plus n k')


def times (n : Nat) (k : Nat) : Nat :=
  match k with
  | Nat.zero => Nat.zero
  | Nat.succ k' => plus n (times n k')

def minus (n : Nat) (k : Nat) : Nat :=
  match k with
  | Nat.zero => n
  | Nat.succ k' => pred (minus n k')
