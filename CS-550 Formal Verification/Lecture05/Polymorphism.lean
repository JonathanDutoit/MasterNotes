/-
  In functional programming, the term polymorphism typically refers
  to datatypes and definitions that take types as arguments
-/

-- Formula takes a type Var as argument and produces another type
inductive Formula1 (Var : Type) : Type
def nat := Formula1 Nat
#check nat


inductive Formula (Var : Type) : Type where
| var (s : Var)
| const (b : Bool)
| al (v: Var) (g : Formula Var)
| ex (v: Var) (g : Formula Var)
| and (left : Formula Var) (right : Formula Var)
| or  (left : Formula Var) (right : Formula Var)
| not (g : Formula Var)

/-
  1. Nat is supplied as the type argument to formula
  2. The constructor Formula.var expects a value of type Nat
  3. The number 3 is of type Nat
  4. Therefore, Formula.var has type Formula Nat
-/
def x : Formula Nat := Formula.var 3
#check (x)
def y : Formula Nat := Formula.var (Var := Nat) 3


structure PPoint (a : Type) where
  x : a
  y : a

def natOrigin : PPoint Nat :=
  { x := Nat.zero, y := Nat.zero }


def replaceX (a : Type) (point : PPoint a) (newX : a) : PPoint a :=
  { point with x := newX }

#check (replaceX)

#check replaceX Nat

#eval replaceX Nat natOrigin 5


inductive Sign where
| pos
| neg

def posOrNegThree (s : Sign) :
    match s with | Sign.pos => Nat | Sign.neg => Int :=
  match s with
  | Sign.pos => (3 : Nat)
  | Sign.neg => (-3 : Int)

/-
  List: linked list datatype
-/


def primesUnder10 : List Nat := [2, 3, 5, 7]

inductive LinkedList (α : Type) where
  | nil : LinkedList α
  | cons : α → LinkedList α → LinkedList α

def explicitPrimesUnder10 : List Nat :=
  List.cons 2 (List.cons 3 (List.cons 5 (List.cons 7 List.nil)))


def length (a : Type) (xs : List a) : Nat :=
  match xs with
  | List.nil => Nat.zero
  | List.cons _ ys => Nat.succ (length a ys)

#eval length String ["Sourdough", "bread"]


def lengthImproved (a : Type) (xs : List a) : Nat :=
  match xs with
  | [] => 0
  | _ :: ys => Nat.succ (length a ys)


/-
  Implicit arguments (curly brackes for type)
-/

def newReplaceX {α : Type} (point : PPoint α) (newX : α) : PPoint α :=
  { point with x := newX }

-- It can be used with natOrigin without providing Nat explicitly
#eval newReplaceX natOrigin 5


/-
  More built-In datatypes
-/

-- Option
inductive LeanOption (α : Type) : Type where
  | none : LeanOption α
  | some (val : α) : LeanOption α


-- Nullable int
#check Option Int

-- Nullable list of strings
#check Option (List String)


-- A Lean naming convention is to define operations that might fail in groups using the suffixes ?
def LeanList.head? {α : Type} (xs : List α) : Option α :=
  match xs with
  | [] => none
  | y :: _ => some y

/-
  A Lean naming convention to define operations:
  - Might fail in groups using the suffixes ? for a version that returns an Option,
  - ! for a version that crashes when provided with invalid input
  -  D for a version that returns a default value when the operation would otherwise fail
-/
#eval primesUnder10.head?
#eval primesUnder10.head!
#eval primesUnder10.headD 10

-- #eval [].head?
#eval [].head? (α := Int)


-- Prod (C#'s tuples)
structure LeanProd {α : Type} {β : Type} : Type where
  fst : α
  snd : β

-- def fives : Prod String Int := {fst := "five", snd := 5}
-- def fives : String × Int := { fst := "five", snd := 5 }
def fives : String × Int := ("five", 5)


/-
  Exercises
-/

def last? {α : Type} (xs : List α) : Option α :=
  match xs with
  | [] => none
  | tail :: [] => some tail
  | _ :: ys =>  last? ys

#eval last? primesUnder10


-- Write a function that finds the first entry in a list that satisfies a given predicate.
def List.findFirst? {α : Type} (xs : List α) (predicate : α → Bool) : Option α :=
  match xs with
  | [] => none
  | y :: ys => if (predicate y) then some y else findFirst? ys predicate


def predicate (n : Nat) := n == 2
#eval primesUnder10.findFirst? predicate

-- Write a function zip that combines two lists into a list of pairs.
-- The resulting list should be as long as the shortest input list
def zip {α β : Type} (xs : List α) (ys : List β) : List (α × β) :=
  match xs, ys with
  | [], _  => List.nil
  | _, [] => List.nil
  | x :: xs', y :: ys' => (x, y) :: zip xs' ys'


def randomText := ["Sourdough", "bread"]
#eval zip primesUnder10 randomText
