Require Import Fcore.
Require Import Gappa_library.
Section Generated_by_Gappa.
Definition f1 := Float2 (1) (1).
Definition f2 := Float2 (-1) (1).
Definition i1 := makepairF f2 f1.
Variable _x : R.
Notation p1 := (BND _x i1). (* BND(x, [-2, 2]) *)
Definition f3 := Float2 (1) (2).
Definition f4 := Float2 (0) (0).
Definition i2 := makepairF f4 f3.
Notation r2 := ((_x * _x)%R).
Notation p2 := (BND r2 i2). (* BND(x * x, [0, 4]) *)
Lemma t1 : p1 -> p2.
 intros h0.
 apply square_o with (1 := h0) ; finalize.
Qed.
Lemma l2 : p1 -> p2. (* BND(x * x, [0, 4]) *)
 intros h0.
 apply t1. exact h0.
Qed.
Lemma l1 : p1 -> p2. (* BND(x * x, [0, 4]) *)
 intros h0.
 apply l2. exact h0.
Qed.
End Generated_by_Gappa.
