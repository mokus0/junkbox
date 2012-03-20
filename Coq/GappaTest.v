Module GappaTest.

Require Import Reals.
Require Import Fcore.
Require Import Gappa_tactic.
Open Scope R_scope.

Definition rnd := rounding_float rndZR 53 (-1074).

Goal
  forall x : R,
  3/4 <= x <= 3 ->
  866 * (powerRZ 2 (-10)) <= rnd (sqrt (rnd x)) <= 1776 * (powerRZ 2 (-10)).
Proof.
  unfold rnd.
  gappa.
Qed.

End GappaTest.