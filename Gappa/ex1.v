Require Import Fcore.
Require Import Gappa_library.
Section Generated_by_Gappa.
Notation r4 := (Float10 (5) (-1)).
Variable _xx : R.
Notation _x := ((round radix2 (FLT_exp (-149) (24)) rndNE)  (_xx)).
Notation r3 := ((_x - r4)%R).
Notation r2 := ((r3 * r3)%R).
Notation r7 := (Float10 (25) (-2)).
Notation r1 := ((r7 - r2)%R).
Notation r10 := (Float1 (1)).
Notation r9 := ((r10 - _x)%R).
Notation _z := ((_x * r9)%R).
Hypothesis a1 : _z = r1.
Lemma b1 : true = true -> _z = r1.
 intros hb.
 apply a1.
Qed.
Definition f1 := Float2 (1) (0).
Definition f2 := Float2 (0) (0).
Definition i1 := makepairF f2 f1.
Notation p1 := (BND _x i1). (* BND(x, [0, 1]) *)
Definition f3 := Float2 (3) (-27).
Definition f4 := Float2 (-3) (-27).
Definition i2 := makepairF f4 f3.
Notation r14 := ((round radix2 (FLT_exp (-149) (24)) rndNE)  (r9)).
Notation r13 := ((_x * r14)%R).
Notation _y := ((round radix2 (FLT_exp (-149) (24)) rndNE)  (r13)).
Notation r11 := ((_y - _z)%R).
Notation p2 := (BND r11 i2). (* BND(y - z, [-2.23517e-08, 2.23517e-08]) *)
Definition f5 := Float2 (1048395) (-21).
Definition i3 := makepairF f2 f5.
Notation p3 := (BND _x i3). (* BND(x, [0, 0.499914]) *)
Notation r16 := ((r13 - _z)%R).
Notation r17 := ((_y - r13)%R).
Notation r15 := ((r17 + r16)%R).
Notation p4 := (BND r15 i2). (* BND(y - x * float<24,-149,ne>(1 - x) + (x * float<24,-149,ne>(1 - x) - z), [-2.23517e-08, 2.23517e-08]) *)
Definition f6 := Float2 (1) (-27).
Definition f7 := Float2 (-1) (-27).
Definition i4 := makepairF f7 f6.
Notation p5 := (BND r17 i4). (* BND(y - x * float<24,-149,ne>(1 - x), [-7.45058e-09, 7.45058e-09]) *)
Definition f8 := Float2 (17592186568635) (-46).
Definition i5 := makepairF f2 f8.
Notation p6 := (ABS r13 i5). (* ABS(x * float<24,-149,ne>(1 - x), [-0, 0.25]) *)
Notation p7 := (BND r13 i5). (* BND(x * float<24,-149,ne>(1 - x), [-0, 0.25]) *)
Definition f9 := Float2 (-1) (-26).
Definition i6 := makepairF f9 f8.
Notation p8 := (BND r13 i6). (* BND(x * float<24,-149,ne>(1 - x), [-1.49012e-08, 0.25]) *)
Notation r20 := ((r14 - r9)%R).
Notation r19 := ((_x * r20)%R).
Notation r18 := ((r19 + _z)%R).
Notation p9 := (BND r18 i6). (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)) + z, [-1.49012e-08, 0.25]) *)
Definition f10 := Float2 (1048395) (-46).
Definition i7 := makepairF f9 f10.
Notation p10 := (BND r19 i7). (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)), [-1.49012e-08, 1.48986e-08]) *)
Definition f11 := Float2 (1) (-25).
Definition f12 := Float2 (-1) (-25).
Definition i8 := makepairF f12 f11.
Notation p11 := (BND r20 i8). (* BND(float<24,-149,ne>(1 - x) - (1 - x), [-2.98023e-08, 2.98023e-08]) *)
Notation p12 := (ABS r9 i1). (* ABS(1 - x, [-0, 1]) *)
Notation p13 := (BND r9 i1). (* BND(1 - x, [-0, 1]) *)
Definition i9 := makepairF f1 f1.
Notation p14 := (BND r10 i9). (* BND(1, [1, 1]) *)
Lemma t1 : p14.
 apply constant1 ; finalize.
Qed.
Lemma l14 : p1 -> p14. (* BND(1, [1, 1]) *)
 intros h0.
 apply t1.
Qed.
Lemma t2 : p14 -> p1 -> p13.
 intros h0 h1.
 apply sub with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l13 : p1 -> p13. (* BND(1 - x, [-0, 1]) *)
 intros h0.
 assert (h1 : p14). apply l14. exact h0.
 apply t2. exact h1. exact h0.
Qed.
Lemma t3 : p13 -> p12.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l12 : p1 -> p12. (* ABS(1 - x, [-0, 1]) *)
 intros h0.
 assert (h1 : p13). apply l13. exact h0.
 apply t3. exact h1.
Qed.
Lemma t4 : p12 -> p11.
 intros h0.
 apply float_absolute_wide_ne with (1 := h0) ; finalize.
Qed.
Lemma l11 : p1 -> p11. (* BND(float<24,-149,ne>(1 - x) - (1 - x), [-2.98023e-08, 2.98023e-08]) *)
 intros h0.
 assert (h1 : p12). apply l12. exact h0.
 apply t4. exact h1.
Qed.
Lemma t5 : p3 -> p11 -> p10.
 intros h0 h1.
 apply mul_po with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l10 : p3 -> p10. (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)), [-1.49012e-08, 1.48986e-08]) *)
 intros h0.
 assert (h1 : p11). apply l11. apply subset with (1 := h0). finalize.
 apply t5. exact h0. exact h1.
Qed.
Definition f13 := Float2 (1099511595015) (-42).
Definition i10 := makepairF f2 f13.
Notation p15 := (BND _z i10). (* BND(z, [-0, 0.25]) *)
Notation p16 := (_z = r1). (* EQL(z, 25e-2 - (x - 5e-1) * (x - 5e-1)) *)
Lemma t6 : p16.
 apply b1 ; finalize.
Qed.
Lemma l16 : p1 -> p16. (* EQL(z, 25e-2 - (x - 5e-1) * (x - 5e-1)) *)
 intros h0.
 apply t6.
Qed.
Notation p17 := (BND r1 i10). (* BND(25e-2 - (x - 5e-1) * (x - 5e-1), [-0, 0.25]) *)
Definition f14 := Float2 (1) (-2).
Definition i11 := makepairF f14 f14.
Notation p18 := (BND r7 i11). (* BND(25e-2, [0.25, 0.25]) *)
Lemma t7 : p18.
 apply constant10 ; finalize.
Qed.
Lemma l18 : p1 -> p18. (* BND(25e-2, [0.25, 0.25]) *)
 intros h0.
 apply t7.
Qed.
Definition f15 := Float2 (32761) (-42).
Definition i12 := makepairF f15 f14.
Notation p19 := (BND r2 i12). (* BND((x - 5e-1) * (x - 5e-1), [7.44899e-09, 0.25]) *)
Definition f16 := Float2 (-181) (-21).
Definition f17 := Float2 (-1) (-1).
Definition i13 := makepairF f17 f16.
Notation p20 := (BND r3 i13). (* BND(x - 5e-1, [-0.5, -8.63075e-05]) *)
Definition f18 := Float2 (1) (-1).
Definition i14 := makepairF f18 f18.
Notation p21 := (BND r4 i14). (* BND(5e-1, [0.5, 0.5]) *)
Lemma t8 : p21.
 apply constant10 ; finalize.
Qed.
Lemma l21 : p1 -> p21. (* BND(5e-1, [0.5, 0.5]) *)
 intros h0.
 apply t8.
Qed.
Lemma t9 : p3 -> p21 -> p20.
 intros h0 h1.
 apply sub with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l20 : p3 -> p20. (* BND(x - 5e-1, [-0.5, -8.63075e-05]) *)
 intros h0.
 assert (h1 : p21). apply l21. apply subset with (1 := h0). finalize.
 apply t9. exact h0. exact h1.
Qed.
Lemma t10 : p20 -> p19.
 intros h0.
 apply square_n with (1 := h0) ; finalize.
Qed.
Lemma l19 : p3 -> p19. (* BND((x - 5e-1) * (x - 5e-1), [7.44899e-09, 0.25]) *)
 intros h0.
 assert (h1 : p20). apply l20. exact h0.
 apply t10. exact h1.
Qed.
Lemma t11 : p18 -> p19 -> p17.
 intros h0 h1.
 apply sub with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l17 : p3 -> p17. (* BND(25e-2 - (x - 5e-1) * (x - 5e-1), [-0, 0.25]) *)
 intros h0.
 assert (h1 : p18). apply l18. apply subset with (1 := h0). finalize.
 assert (h2 : p19). apply l19. exact h0.
 apply t11. exact h1. exact h2.
Qed.
Lemma t12 : p16 -> p17 -> p15.
 intros h0 h1.
 apply bnd_rewrite with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l15 : p3 -> p15. (* BND(z, [-0, 0.25]) *)
 intros h0.
 assert (h1 : p16). apply l16. apply subset with (1 := h0). finalize.
 assert (h2 : p17). apply l17. exact h0.
 apply t12. exact h1. exact h2.
Qed.
Lemma t13 : p10 -> p15 -> p9.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l9 : p3 -> p9. (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)) + z, [-1.49012e-08, 0.25]) *)
 intros h0.
 assert (h1 : p10). apply l10. exact h0.
 assert (h2 : p15). apply l15. exact h0.
 apply t13. exact h1. exact h2.
Qed.
Lemma t14 : p9 -> p8.
 intros h0.
 apply mul_xars with (1 := h0) ; finalize.
Qed.
Lemma l8 : p3 -> p8. (* BND(x * float<24,-149,ne>(1 - x), [-1.49012e-08, 0.25]) *)
 intros h0.
 assert (h1 : p9). apply l9. exact h0.
 apply t14. exact h1.
Qed.
Definition f19 := Float2 (16777217) (-26).
Definition i15 := makepairF f2 f19.
Notation p22 := (BND r13 i15). (* BND(x * float<24,-149,ne>(1 - x), [-0, 0.25]) *)
Definition i16 := makepairF f2 f18.
Notation p23 := (BND _x i16). (* BND(x, [0, 0.5]) *)
Definition i17 := makepairF f12 f19.
Notation p24 := (BND r13 i17). (* BND(x * float<24,-149,ne>(1 - x), [-2.98023e-08, 0.25]) *)
Notation p25 := (BND r18 i17). (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)) + z, [-2.98023e-08, 0.25]) *)
Definition f20 := Float2 (1) (-26).
Definition i18 := makepairF f9 f20.
Notation p26 := (BND r19 i18). (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)), [-1.49012e-08, 1.49012e-08]) *)
Lemma t15 : p23 -> p11 -> p26.
 intros h0 h1.
 apply mul_po with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l26 : p23 -> p26. (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)), [-1.49012e-08, 1.49012e-08]) *)
 intros h0.
 assert (h1 : p11). apply l11. apply subset with (1 := h0). finalize.
 apply t15. exact h0. exact h1.
Qed.
Definition i19 := makepairF f2 f14.
Notation p27 := (BND _z i19). (* BND(z, [-0, 0.25]) *)
Notation p28 := (BND r1 i19). (* BND(25e-2 - (x - 5e-1) * (x - 5e-1), [-0, 0.25]) *)
Notation p29 := (BND r2 i19). (* BND((x - 5e-1) * (x - 5e-1), [0, 0.25]) *)
Definition i20 := makepairF f17 f18.
Notation p30 := (BND r3 i20). (* BND(x - 5e-1, [-0.5, 0.5]) *)
Lemma t16 : p1 -> p21 -> p30.
 intros h0 h1.
 apply sub with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l30 : p1 -> p30. (* BND(x - 5e-1, [-0.5, 0.5]) *)
 intros h0.
 assert (h1 : p21). apply l21. exact h0.
 apply t16. exact h0. exact h1.
Qed.
Lemma t17 : p30 -> p29.
 intros h0.
 apply square_o with (1 := h0) ; finalize.
Qed.
Lemma l29 : p1 -> p29. (* BND((x - 5e-1) * (x - 5e-1), [0, 0.25]) *)
 intros h0.
 assert (h1 : p30). apply l30. exact h0.
 apply t17. exact h1.
Qed.
Lemma t18 : p18 -> p29 -> p28.
 intros h0 h1.
 apply sub with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l28 : p1 -> p28. (* BND(25e-2 - (x - 5e-1) * (x - 5e-1), [-0, 0.25]) *)
 intros h0.
 assert (h1 : p18). apply l18. exact h0.
 assert (h2 : p29). apply l29. exact h0.
 apply t18. exact h1. exact h2.
Qed.
Lemma t19 : p16 -> p28 -> p27.
 intros h0 h1.
 apply bnd_rewrite with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l27 : p1 -> p27. (* BND(z, [-0, 0.25]) *)
 intros h0.
 assert (h1 : p16). apply l16. exact h0.
 assert (h2 : p28). apply l28. exact h0.
 apply t19. exact h1. exact h2.
Qed.
Definition i21 := makepairF f12 f20.
Notation p31 := (BND r19 i21). (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)), [-2.98023e-08, 1.49012e-08]) *)
Lemma t20 : p31 -> p27 -> p25.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l25 : p23 -> p25. (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)) + z, [-2.98023e-08, 0.25]) *)
 intros h0.
 assert (h1 : p26). apply l26. exact h0.
 assert (h2 : p27). apply l27. apply subset with (1 := h0). finalize.
 apply t20. apply subset with (1 := h1). finalize. exact h2.
Qed.
Lemma t21 : p25 -> p24.
 intros h0.
 apply mul_xars with (1 := h0) ; finalize.
Qed.
Lemma l24 : p23 -> p24. (* BND(x * float<24,-149,ne>(1 - x), [-2.98023e-08, 0.25]) *)
 intros h0.
 assert (h1 : p25). apply l25. exact h0.
 apply t21. exact h1.
Qed.
Notation p32 := (BND r13 i1). (* BND(x * float<24,-149,ne>(1 - x), [-0, 1]) *)
Notation p33 := (BND r14 i1). (* BND(float<24,-149,ne>(1 - x), [-0, 1]) *)
Lemma t22 : p13 -> p33.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l32 : p1 -> p33. (* BND(float<24,-149,ne>(1 - x), [-0, 1]) *)
 intros h0.
 assert (h1 : p13). apply l13. exact h0.
 apply t22. exact h1.
Qed.
Lemma t23 : p1 -> p33 -> p32.
 intros h0 h1.
 apply mul_pp with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l31 : p1 -> p32. (* BND(x * float<24,-149,ne>(1 - x), [-0, 1]) *)
 intros h0.
 assert (h1 : p33). apply l32. exact h0.
 apply t23. exact h0. exact h1.
Qed.
Lemma l23 : p23 -> p22. (* BND(x * float<24,-149,ne>(1 - x), [-0, 0.25]) *)
 intros h0.
 assert (h1 : p24). apply l24. exact h0.
 assert (h2 : p32). apply l31. apply subset with (1 := h0). finalize.
 apply intersect with (1 := h1) (2 := h2). finalize.
Qed.
Definition i22 := makepairF f18 f1.
Notation p34 := (BND _x i22). (* BND(x, [0.5, 1]) *)
Notation p35 := (BND r20 i18). (* BND(float<24,-149,ne>(1 - x) - (1 - x), [-1.49012e-08, 1.49012e-08]) *)
Notation p36 := (ABS r9 i16). (* ABS(1 - x, [-0, 0.5]) *)
Notation p37 := (BND r9 i16). (* BND(1 - x, [-0, 0.5]) *)
Lemma t24 : p14 -> p34 -> p37.
 intros h0 h1.
 apply sub with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l39 : p34 -> p37. (* BND(1 - x, [-0, 0.5]) *)
 intros h0.
 assert (h1 : p14). apply l14. apply subset with (1 := h0). finalize.
 apply t24. exact h1. exact h0.
Qed.
Lemma t25 : p37 -> p36.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l38 : p34 -> p36. (* ABS(1 - x, [-0, 0.5]) *)
 intros h0.
 assert (h1 : p37). apply l39. exact h0.
 apply t25. exact h1.
Qed.
Lemma t26 : p36 -> p35.
 intros h0.
 apply float_absolute_wide_ne with (1 := h0) ; finalize.
Qed.
Lemma l37 : p34 -> p35. (* BND(float<24,-149,ne>(1 - x) - (1 - x), [-1.49012e-08, 1.49012e-08]) *)
 intros h0.
 assert (h1 : p36). apply l38. exact h0.
 apply t26. exact h1.
Qed.
Lemma t27 : p34 -> p35 -> p26.
 intros h0 h1.
 apply mul_po with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l36 : p34 -> p26. (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)), [-1.49012e-08, 1.49012e-08]) *)
 intros h0.
 assert (h1 : p35). apply l37. exact h0.
 apply t27. exact h0. exact h1.
Qed.
Lemma t28 : p31 -> p27 -> p25.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l35 : p34 -> p25. (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)) + z, [-2.98023e-08, 0.25]) *)
 intros h0.
 assert (h1 : p26). apply l36. exact h0.
 assert (h2 : p27). apply l27. apply subset with (1 := h0). finalize.
 apply t28. apply subset with (1 := h1). finalize. exact h2.
Qed.
Lemma t29 : p25 -> p24.
 intros h0.
 apply mul_xars with (1 := h0) ; finalize.
Qed.
Lemma l34 : p34 -> p24. (* BND(x * float<24,-149,ne>(1 - x), [-2.98023e-08, 0.25]) *)
 intros h0.
 assert (h1 : p25). apply l35. exact h0.
 apply t29. exact h1.
Qed.
Lemma l33 : p34 -> p22. (* BND(x * float<24,-149,ne>(1 - x), [-0, 0.25]) *)
 intros h0.
 assert (h1 : p24). apply l34. exact h0.
 assert (h2 : p32). apply l31. apply subset with (1 := h0). finalize.
 apply intersect with (1 := h1) (2 := h2). finalize.
Qed.
Lemma l22 : p1 -> p22. (* BND(x * float<24,-149,ne>(1 - x), [-0, 0.25]) *)
 intros h0.
 generalize h0. clear h0.
 assert (u : p23 -> p22). intro h0. (* [0, 0.5] *)
 apply l23. exact h0.
 next_interval (union) u.
 assert (u : p34 -> p22). intro h0. (* [0.5, 1] *)
 apply l33. exact h0.
 exact u.
Qed.
Lemma l7 : p3 -> p7. (* BND(x * float<24,-149,ne>(1 - x), [-0, 0.25]) *)
 intros h0.
 assert (h1 : p8). apply l8. exact h0.
 assert (h2 : p22). apply l22. apply subset with (1 := h0). finalize.
 apply intersect with (1 := h1) (2 := h2). finalize.
Qed.
Lemma t30 : p7 -> p6.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l6 : p3 -> p6. (* ABS(x * float<24,-149,ne>(1 - x), [-0, 0.25]) *)
 intros h0.
 assert (h1 : p7). apply l7. exact h0.
 apply t30. exact h1.
Qed.
Lemma t31 : p6 -> p5.
 intros h0.
 apply float_absolute_wide_ne with (1 := h0) ; finalize.
Qed.
Lemma l5 : p3 -> p5. (* BND(y - x * float<24,-149,ne>(1 - x), [-7.45058e-09, 7.45058e-09]) *)
 intros h0.
 assert (h1 : p6). apply l6. exact h0.
 apply t31. exact h1.
Qed.
Notation p38 := (BND r16 i18). (* BND(x * float<24,-149,ne>(1 - x) - z, [-1.49012e-08, 1.49012e-08]) *)
Lemma t32 : p26 -> p38.
 intros h0.
 apply mul_fils with (1 := h0) ; finalize.
Qed.
Lemma l40 : p3 -> p38. (* BND(x * float<24,-149,ne>(1 - x) - z, [-1.49012e-08, 1.49012e-08]) *)
 intros h0.
 assert (h1 : p10). apply l10. exact h0.
 apply t32. apply subset with (1 := h1). finalize.
Qed.
Lemma t33 : p5 -> p38 -> p4.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l4 : p3 -> p4. (* BND(y - x * float<24,-149,ne>(1 - x) + (x * float<24,-149,ne>(1 - x) - z), [-2.23517e-08, 2.23517e-08]) *)
 intros h0.
 assert (h1 : p5). apply l5. exact h0.
 assert (h2 : p38). apply l40. exact h0.
 apply t33. exact h1. exact h2.
Qed.
Lemma t34 : p4 -> p2.
 intros h0.
 apply sub_xals with (1 := h0) ; finalize.
Qed.
Lemma l3 : p3 -> p2. (* BND(y - z, [-2.23517e-08, 2.23517e-08]) *)
 intros h0.
 assert (h1 : p4). apply l4. exact h0.
 apply t34. exact h1.
Qed.
Definition i23 := makepairF f5 f18.
Notation p39 := (BND _x i23). (* BND(x, [0.499914, 0.5]) *)
Definition f21 := Float2 (16777215) (-26).
Definition i24 := makepairF f21 f14.
Notation p40 := (BND _y i24). (* BND(y, [0.25, 0.25]) *)
Definition f22 := Float2 (1099511529479) (-42).
Definition i25 := makepairF f22 f19.
Notation p41 := (BND r13 i25). (* BND(x * float<24,-149,ne>(1 - x), [0.25, 0.25]) *)
Notation p42 := (BND r18 i25). (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)) + z, [0.25, 0.25]) *)
Lemma l45 : p1 -> p26. (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)), [-1.49012e-08, 1.49012e-08]) *)
 intros h0.
 generalize h0. clear h0.
 assert (u : p23 -> p26). intro h0. (* [0, 0.5] *)
 apply l26. exact h0.
 next_interval (union) u.
 assert (u : p34 -> p26). intro h0. (* [0.5, 1] *)
 apply l36. exact h0.
 exact u.
Qed.
Definition i26 := makepairF f13 f14.
Notation p43 := (BND _z i26). (* BND(z, [0.25, 0.25]) *)
Notation p44 := (BND r1 i26). (* BND(25e-2 - (x - 5e-1) * (x - 5e-1), [0.25, 0.25]) *)
Definition i27 := makepairF f2 f15.
Notation p45 := (BND r2 i27). (* BND((x - 5e-1) * (x - 5e-1), [0, 7.44899e-09]) *)
Definition i28 := makepairF f16 f2.
Notation p46 := (BND r3 i28). (* BND(x - 5e-1, [-8.63075e-05, 0]) *)
Lemma t35 : p39 -> p21 -> p46.
 intros h0 h1.
 apply sub with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l49 : p39 -> p46. (* BND(x - 5e-1, [-8.63075e-05, 0]) *)
 intros h0.
 assert (h1 : p21). apply l21. apply subset with (1 := h0). finalize.
 apply t35. exact h0. exact h1.
Qed.
Lemma t36 : p46 -> p45.
 intros h0.
 apply square_n with (1 := h0) ; finalize.
Qed.
Lemma l48 : p39 -> p45. (* BND((x - 5e-1) * (x - 5e-1), [0, 7.44899e-09]) *)
 intros h0.
 assert (h1 : p46). apply l49. exact h0.
 apply t36. exact h1.
Qed.
Lemma t37 : p18 -> p45 -> p44.
 intros h0 h1.
 apply sub with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l47 : p39 -> p44. (* BND(25e-2 - (x - 5e-1) * (x - 5e-1), [0.25, 0.25]) *)
 intros h0.
 assert (h1 : p18). apply l18. apply subset with (1 := h0). finalize.
 assert (h2 : p45). apply l48. exact h0.
 apply t37. exact h1. exact h2.
Qed.
Lemma t38 : p16 -> p44 -> p43.
 intros h0 h1.
 apply bnd_rewrite with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l46 : p39 -> p43. (* BND(z, [0.25, 0.25]) *)
 intros h0.
 assert (h1 : p16). apply l16. apply subset with (1 := h0). finalize.
 assert (h2 : p44). apply l47. exact h0.
 apply t38. exact h1. exact h2.
Qed.
Lemma t39 : p26 -> p43 -> p42.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l44 : p39 -> p42. (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)) + z, [0.25, 0.25]) *)
 intros h0.
 assert (h1 : p26). apply l45. apply subset with (1 := h0). finalize.
 assert (h2 : p43). apply l46. exact h0.
 apply t39. exact h1. exact h2.
Qed.
Lemma t40 : p42 -> p41.
 intros h0.
 apply mul_xars with (1 := h0) ; finalize.
Qed.
Lemma l43 : p39 -> p41. (* BND(x * float<24,-149,ne>(1 - x), [0.25, 0.25]) *)
 intros h0.
 assert (h1 : p42). apply l44. exact h0.
 apply t40. exact h1.
Qed.
Lemma t41 : p41 -> p40.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l42 : p39 -> p40. (* BND(y, [0.25, 0.25]) *)
 intros h0.
 assert (h1 : p41). apply l43. exact h0.
 apply t41. exact h1.
Qed.
Notation p47 := (BND _z i24). (* BND(z, [0.25, 0.25]) *)
Lemma t42 : p40 -> p47 -> p2.
 intros h0 h1.
 apply sub with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l41 : p39 -> p2. (* BND(y - z, [-2.23517e-08, 2.23517e-08]) *)
 intros h0.
 assert (h1 : p40). apply l42. exact h0.
 assert (h2 : p43). apply l46. exact h0.
 apply t42. exact h1. apply subset with (1 := h2). finalize.
Qed.
Notation p48 := (BND r17 i18). (* BND(y - x * float<24,-149,ne>(1 - x), [-1.49012e-08, 1.49012e-08]) *)
Definition f23 := Float2 (8388609) (-25).
Definition i29 := makepairF f2 f23.
Notation p49 := (ABS _y i29). (* ABS(y, [-0, 0.25]) *)
Notation p50 := (BND _y i29). (* BND(y, [-0, 0.25]) *)
Notation p51 := (BND r13 i29). (* BND(x * float<24,-149,ne>(1 - x), [-0, 0.25]) *)
Definition f24 := Float2 (-1) (-24).
Definition i30 := makepairF f24 f23.
Notation p52 := (BND r13 i30). (* BND(x * float<24,-149,ne>(1 - x), [-5.96046e-08, 0.25]) *)
Notation p53 := (BND r18 i30). (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)) + z, [-5.96046e-08, 0.25]) *)
Definition i31 := makepairF f24 f11.
Notation p54 := (BND r19 i31). (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)), [-5.96046e-08, 2.98023e-08]) *)
Notation p55 := (BND r20 i31). (* BND(float<24,-149,ne>(1 - x) - (1 - x), [-5.96046e-08, 2.98023e-08]) *)
Lemma t43 : p1 -> p55 -> p54.
 intros h0 h1.
 apply mul_po with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l58 : p1 -> p54. (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)), [-5.96046e-08, 2.98023e-08]) *)
 intros h0.
 assert (h1 : p11). apply l11. exact h0.
 apply t43. exact h0. apply subset with (1 := h1). finalize.
Qed.
Lemma t44 : p54 -> p27 -> p53.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l57 : p1 -> p53. (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)) + z, [-5.96046e-08, 0.25]) *)
 intros h0.
 assert (h1 : p54). apply l58. exact h0.
 assert (h2 : p27). apply l27. exact h0.
 apply t44. exact h1. exact h2.
Qed.
Lemma t45 : p53 -> p52.
 intros h0.
 apply mul_xars with (1 := h0) ; finalize.
Qed.
Lemma l56 : p1 -> p52. (* BND(x * float<24,-149,ne>(1 - x), [-5.96046e-08, 0.25]) *)
 intros h0.
 assert (h1 : p53). apply l57. exact h0.
 apply t45. exact h1.
Qed.
Lemma l55 : p1 -> p51. (* BND(x * float<24,-149,ne>(1 - x), [-0, 0.25]) *)
 intros h0.
 assert (h1 : p52). apply l56. exact h0.
 assert (h2 : p32). apply l31. exact h0.
 apply intersect with (1 := h1) (2 := h2). finalize.
Qed.
Lemma t46 : p51 -> p50.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l54 : p1 -> p50. (* BND(y, [-0, 0.25]) *)
 intros h0.
 assert (h1 : p51). apply l55. exact h0.
 apply t46. exact h1.
Qed.
Lemma t47 : p50 -> p49.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l53 : p1 -> p49. (* ABS(y, [-0, 0.25]) *)
 intros h0.
 assert (h1 : p50). apply l54. exact h0.
 apply t47. exact h1.
Qed.
Lemma t48 : p49 -> p48.
 intros h0.
 apply (float_absolute_inv_ne (24) (-149)) with (1 := h0) ; finalize.
Qed.
Lemma l52 : p1 -> p48. (* BND(y - x * float<24,-149,ne>(1 - x), [-1.49012e-08, 1.49012e-08]) *)
 intros h0.
 assert (h1 : p49). apply l53. exact h0.
 apply t48. exact h1.
Qed.
Definition i32 := makepairF f2 f2.
Notation p56 := (BND r16 i32). (* BND(x * float<24,-149,ne>(1 - x) - z, [0, 0]) *)
Notation p57 := (BND r19 i32). (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)), [0, 0]) *)
Notation p58 := (BND r20 i32). (* BND(float<24,-149,ne>(1 - x) - (1 - x), [0, 0]) *)
Notation p59 := (r14 = r9). (* EQL(float<24,-149,ne>(1 - x), 1 - x) *)
Notation p60 := (FIX r9 (-24)). (* FIX(1 - x, -24) *)
Notation p61 := (FIX r10 (0)). (* FIX(1, 0) *)
Notation p62 := (ABS r10 i9). (* ABS(1, [1, 1]) *)
Lemma t49 : p14 -> p62.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l65 : p1 -> p62. (* ABS(1, [1, 1]) *)
 intros h0.
 assert (h1 : p14). apply l14. exact h0.
 apply t49. exact h1.
Qed.
Lemma t50 : p62 -> p61.
 intros h0.
 apply fix_of_singleton_bnd with (1 := h0) ; finalize.
Qed.
Lemma l64 : p1 -> p61. (* FIX(1, 0) *)
 intros h0.
 assert (h1 : p62). apply l65. exact h0.
 apply t50. exact h1.
Qed.
Notation p63 := (FIX _x (-24)). (* FIX(x, -24) *)
Notation p64 := (FLT _x (24)). (* FLT(x, 24) *)
Lemma t51 : p64.
 apply flt_of_float ; finalize.
Qed.
Lemma l67 : p1 -> p64. (* FLT(x, 24) *)
 intros h0.
 apply t51.
Qed.
Notation p65 := (ABS _x i22). (* ABS(x, [0.5, 1]) *)
Lemma t52 : p34 -> p65.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l68 : p34 -> p65. (* ABS(x, [0.5, 1]) *)
 intros h0.
 apply t52. exact h0.
Qed.
Lemma t53 : p64 -> p65 -> p63.
 intros h0 h1.
 apply fix_of_flt_bnd with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l66 : p34 -> p63. (* FIX(x, -24) *)
 intros h0.
 assert (h1 : p64). apply l67. apply subset with (1 := h0). finalize.
 assert (h2 : p65). apply l68. exact h0.
 apply t53. exact h1. exact h2.
Qed.
Lemma t54 : p61 -> p63 -> p60.
 intros h0 h1.
 apply sub_fix with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l63 : p34 -> p60. (* FIX(1 - x, -24) *)
 intros h0.
 assert (h1 : p61). apply l64. apply subset with (1 := h0). finalize.
 assert (h2 : p63). apply l66. exact h0.
 apply t54. exact h1. exact h2.
Qed.
Notation p66 := (FLT r9 (24)). (* FLT(1 - x, 24) *)
Lemma t55 : p14 -> p34 -> p37.
 intros h0 h1.
 apply sub with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l71 : p34 -> p37. (* BND(1 - x, [-0, 0.5]) *)
 intros h0.
 assert (h1 : p14). apply l14. apply subset with (1 := h0). finalize.
 apply t55. exact h1. exact h0.
Qed.
Lemma t56 : p37 -> p36.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l70 : p34 -> p36. (* ABS(1 - x, [-0, 0.5]) *)
 intros h0.
 assert (h1 : p37). apply l71. exact h0.
 apply t56. exact h1.
Qed.
Lemma t57 : p60 -> p36 -> p66.
 intros h0 h1.
 apply flt_of_fix_bnd with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l69 : p34 -> p66. (* FLT(1 - x, 24) *)
 intros h0.
 assert (h1 : p60). apply l63. exact h0.
 assert (h2 : p36). apply l70. exact h0.
 apply t57. exact h1. exact h2.
Qed.
Lemma t58 : p60 -> p66 -> p59.
 intros h0 h1.
 apply float_of_fix_flt with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l62 : p34 -> p59. (* EQL(float<24,-149,ne>(1 - x), 1 - x) *)
 intros h0.
 assert (h1 : p60). apply l63. exact h0.
 assert (h2 : p66). apply l69. exact h0.
 apply t58. exact h1. exact h2.
Qed.
Lemma t59 : p59 -> p58.
 intros h0.
 apply sub_of_eql with (1 := h0) ; finalize.
Qed.
Lemma l61 : p34 -> p58. (* BND(float<24,-149,ne>(1 - x) - (1 - x), [0, 0]) *)
 intros h0.
 assert (h1 : p59). apply l62. exact h0.
 apply t59. exact h1.
Qed.
Lemma t60 : p34 -> p58 -> p57.
 intros h0 h1.
 apply mul_pp with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l60 : p34 -> p57. (* BND(x * (float<24,-149,ne>(1 - x) - (1 - x)), [0, 0]) *)
 intros h0.
 assert (h1 : p58). apply l61. exact h0.
 apply t60. exact h0. exact h1.
Qed.
Lemma t61 : p57 -> p56.
 intros h0.
 apply mul_fils with (1 := h0) ; finalize.
Qed.
Lemma l59 : p34 -> p56. (* BND(x * float<24,-149,ne>(1 - x) - z, [0, 0]) *)
 intros h0.
 assert (h1 : p57). apply l60. exact h0.
 apply t61. exact h1.
Qed.
Lemma t62 : p48 -> p56 -> p4.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l51 : p34 -> p4. (* BND(y - x * float<24,-149,ne>(1 - x) + (x * float<24,-149,ne>(1 - x) - z), [-2.23517e-08, 2.23517e-08]) *)
 intros h0.
 assert (h1 : p48). apply l52. apply subset with (1 := h0). finalize.
 assert (h2 : p56). apply l59. exact h0.
 apply t62. exact h1. exact h2.
Qed.
Lemma t63 : p4 -> p2.
 intros h0.
 apply sub_xals with (1 := h0) ; finalize.
Qed.
Lemma l50 : p34 -> p2. (* BND(y - z, [-2.23517e-08, 2.23517e-08]) *)
 intros h0.
 assert (h1 : p4). apply l51. exact h0.
 apply t63. exact h1.
Qed.
Lemma l2 : p1 -> p2. (* BND(y - z, [-2.23517e-08, 2.23517e-08]) *)
 intros h0.
 generalize h0. clear h0.
 assert (u : p3 -> p2). intro h0. (* [0, 0.499914] *)
 apply l3. exact h0.
 next_interval (union) u.
 assert (u : p39 -> p2). intro h0. (* [0.499914, 0.5] *)
 apply l41. exact h0.
 next_interval (union) u.
 assert (u : p34 -> p2). intro h0. (* [0.5, 1] *)
 apply l50. exact h0.
 exact u.
Qed.
Lemma l1 : p1 -> p2. (* BND(y - z, [-2.23517e-08, 2.23517e-08]) *)
 intros h0.
 apply l2. exact h0.
Qed.
Notation p67 := (BND _y i19). (* BND(y, [0, 0.25]) *)
Lemma t64 : p22 -> p67.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l74 : p23 -> p67. (* BND(y, [-0, 0.25]) *)
 intros h0.
 assert (h1 : p22). apply l23. exact h0.
 apply t64. exact h1.
Qed.
Lemma t65 : p22 -> p67.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l75 : p34 -> p67. (* BND(y, [-0, 0.25]) *)
 intros h0.
 assert (h1 : p22). apply l33. exact h0.
 apply t65. exact h1.
Qed.
Lemma l73 : p1 -> p67. (* BND(y, [-0, 0.25]) *)
 intros h0.
 generalize h0. clear h0.
 assert (u : p23 -> p67). intro h0. (* [0, 0.5] *)
 apply l74. exact h0.
 next_interval (union) u.
 assert (u : p34 -> p67). intro h0. (* [0.5, 1] *)
 apply l75. exact h0.
 exact u.
Qed.
Lemma l72 : p1 -> p67. (* BND(y, [0, 0.25]) *)
 intros h0.
 apply l73. exact h0.
Qed.
End Generated_by_Gappa.
