#check Nat.mod_lt
#check Nat.pos_of_ne_zero

def gcd (a b : Nat) : Nat :=
  if isZero : a = 0 then b
  else
    let r := b % a
    gcd r a
-- Every time I call the function recursively, the first argument gets smaller.
-- Since natural numbers cannot decrease forever, the recursion must terminate.
termination_by a
decreasing_by
  have aPos: 0 < a := Nat.pos_of_ne_zero isZero
  exact Nat.mod_lt (x := b) aPos

#print gcd.eq_def

theorem modLess {a b : Nat} (h : a < b) : a % b = a := Nat.mod_eq_of_lt h

theorem gcdSym_swap (a b : Nat) (h: a < b) : gcd a b = gcd b a := by
  grind[modLess, gcd]

theorem gcdSym (a: Nat) (b: Nat) : gcd a b = gcd b a := by
  have tri: a = b ∨ a < b ∨ a > b := by grind[Nat.lt_trichotomy]
  rcases tri with _ | _ | _
  · have IH : (a = b) := by assumption
    rw[IH]
  · have IH : (a < b) := by assumption
    rw[gcdSym_swap]
    exact IH
  · have IH : (a > b) := by assumption
    rw[gcdSym_swap b a IH]

theorem divisionLemma {k m n : Nat} (h : k ∣ n) :
   k ∣ m % n ↔ k ∣ m  := Nat.dvd_mod_iff h

theorem gcdDivides (a: Nat): ∀ (b : Nat), gcd a b ∣ a ∧ gcd a b ∣ b := by
  induction a using Nat.strongRecOn with
  | ind a ih =>
    intro b
    by_cases isZero : a = 0
    · simp[isZero, gcd]
    · have h : a ≠ 0 := by assumption
      let r := b % a
      have s : gcd a b = gcd r a := by grind[gcd]
      suffices gcd r a ∣ a  ∧  gcd r a ∣ b from by grind
      have remainderLess : r < a := Nat.mod_lt b (Nat.pos_of_ne_zero h)
      have ⟨goal1, goal2⟩ := ih (m := r) remainderLess (b := a)
      have goal1:        gcd r a ∣ a  := by assumption
      have divRemainder: gcd r a ∣ r  := by assumption
      have goal2:        gcd r a ∣ b  := by grind [divisionLemma]
      exact ⟨goal1, goal2⟩
