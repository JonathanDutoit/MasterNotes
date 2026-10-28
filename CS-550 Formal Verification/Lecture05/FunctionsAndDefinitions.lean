def hello := "Hello"
def lean : String := "Lean"
def lean2 : Int := 2
#eval String.append hello (String.append " " lean)

def add1 (n : Nat) : Nat := n + 1
#eval add1 7
#check add1

/-
  However, Lean can be “tricked” into showing the function's type
  by writing the function's name in parentheses,
  which causes the function to be treated as an ordinary expression
-/
#check (add1)

/-
  Behind the scenes, all functions actually expect precisely one argument.
  Functions like maximum that seem to take more than one argument are in
  fact functions that take one argument and then return a new function.
-/

def maximum (n : Nat) (k : Nat) : Nat :=
  if n < k then
    k
  else n

#eval maximum 6 5
#check (maximum)

def spaceBetween (before : String) (after : String) : String :=
  String.append before (String.append " " after)


-- Exercices
def joinStringWith (first : String) (second : String) (third : String) : String :=
  String.append second (String.append first third)
#eval joinStringWith ", " "one" "and another"
#check joinStringWith
#check (joinStringWith)

-- computes the volume of a rectangular prism with the given height, width, and depth
def volume (height : Nat) (width : Nat) (depth : Nat) : Nat :=
  height * width * depth

#eval volume 4 3 5

def Str : Type := String
def aStr : Str := "This is a string."
#eval aStr

-- Lean allows number literals to be overloaded
def NaturalNumber : Type := Nat
-- def thirtyEight : NaturalNumber := 38
def thirtyEight : NaturalNumber := (38 : Nat)


/-
  Defining the new name for Nat using abbrev instead of
  def allows overloading resolution to replace the defined
  name with its definition.

  Definitions written using abbrev are always unfolded.
  Definitions produced with abbrev are marked as reducible.
-/
abbrev N : Type := Nat
def thirtyNine : N := 39
#eval thirtyNine
