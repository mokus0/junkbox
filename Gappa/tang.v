Require Import Fcore.
Require Import Gappa_library.
Section Generated_by_Gappa.
Variable _Z : R.
Variable _R0 : R.
Notation _a2 := (Float2 (2796219) (-24)).
Notation r10 := ((_a2 * _R0)%R).
Notation r9 := ((r10 * _R0)%R).
Notation r7 := ((r9 * _R0)%R).
Notation _a1 := (Float2 (2097169) (-22)).
Notation r14 := ((_a1 * _R0)%R).
Notation r13 := ((r14 * _R0)%R).
Notation r17 := (Float1 (1)).
Notation r16 := ((r17 + _R0)%R).
Notation r12 := ((r16 + r13)%R).
Notation r6 := ((r12 + r7)%R).
Notation r4 := ((r6 + _Z)%R).
Variable _S0 : R.
Notation _E0 := ((_S0 * r4)%R).
Notation r21 := (Float1 0).
Notation _l2 := (Float2 (6283079) (-47)).
Variable _n : R.
Notation r28 := ((- _n)%R).
Notation r26 := ((r28 * _l2)%R).
Notation _r2 := ((round radix2 (FLT_exp (-149) (24)) rndNE)  (r26)).
Variable _r1 : R.
Notation _R := ((_r1 + _r2)%R).
Notation r32 := ((_a2 * _R)%R).
Notation r31 := ((r32 * _R)%R).
Notation r23 := ((r31 * _R)%R).
Notation r35 := ((_a1 * _R)%R).
Notation r34 := ((r35 * _R)%R).
Notation r36 := ((r17 + _R)%R).
Notation r33 := ((r36 + r34)%R).
Notation r22 := ((r33 + r23)%R).
Notation r20 := ((r22 + r21)%R).
Notation _s2 := (Float2 (13833605) (-44)).
Notation _s1 := (Float2 (66971) (-16)).
Notation _S := ((_s1 + _s2)%R).
Notation _Er := ((_S * r20)%R).
Notation r2 := ((_Er - _E0)%R).
Notation r48 := ((_R * _a2)%R).
Notation r47 := ((_a1 + r48)%R).
Notation r49 := ((_R * _R)%R).
Notation r46 := ((r49 * r47)%R).
Notation r45 := ((_r2 + r46)%R).
Notation r44 := ((_r1 + r45)%R).
Notation r43 := ((_S * r44)%R).
Notation r42 := ((_s2 + r43)%R).
Notation _E := ((_s1 + r42)%R).
Notation _r := ((round radix2 (FLT_exp (-149) (24)) rndNE)  (_R)).
Notation r65 := ((_r * _a2)%R).
Notation r64 := ((round radix2 (FLT_exp (-149) (24)) rndNE)  (r65)).
Notation r63 := ((_a1 + r64)%R).
Notation r62 := ((round radix2 (FLT_exp (-149) (24)) rndNE)  (r63)).
Notation r68 := ((_r * _r)%R).
Notation r67 := ((round radix2 (FLT_exp (-149) (24)) rndNE)  (r68)).
Notation r61 := ((r67 * r62)%R).
Notation _q := ((round radix2 (FLT_exp (-149) (24)) rndNE)  (r61)).
Notation r59 := ((_r2 + _q)%R).
Notation r58 := ((round radix2 (FLT_exp (-149) (24)) rndNE)  (r59)).
Notation r57 := ((_r1 + r58)%R).
Notation _p := ((round radix2 (FLT_exp (-149) (24)) rndNE)  (r57)).
Notation _s := ((round radix2 (FLT_exp (-149) (24)) rndNE)  (_S)).
Notation r55 := ((_s * _p)%R).
Notation r54 := ((round radix2 (FLT_exp (-149) (24)) rndNE)  (r55)).
Notation r53 := ((_s2 + r54)%R).
Notation r52 := ((round radix2 (FLT_exp (-149) (24)) rndNE)  (r53)).
Notation r51 := ((_s1 + r52)%R).
Notation _e := ((round radix2 (FLT_exp (-149) (24)) rndNE)  (r51)).
Notation r40 := ((_e - _E)%R).
Notation r1 := ((r40 + r2)%R).
Notation r70 := ((_e - _E0)%R).
Hypothesis a1 : r70 = r1.
Lemma b1 : true = true -> r70 = r1.
 intros hb.
 apply a1.
Qed.
Notation r71 := ((_R - _r2)%R).
Hypothesis a2 : _r1 = r71.
Lemma b2 : true = true -> _r1 = r71.
 intros hb.
 apply a2.
Qed.
Definition f1 := Float2 (159) (6).
Definition f2 := Float2 (-159) (6).
Definition i1 := makepairF f2 f1.
Notation p1 := (BND _n i1). (* BND(n, [-10176, 10176]) *)
Definition f3 := Float2 (800588692798994541) (-65).
Definition f4 := Float2 (0) (0).
Definition i2 := makepairF f4 f3.
Notation p2 := (BND _R i2). (* BND(R, [0, 0.0217]) *)
Definition f5 := Float2 (55) (-39).
Definition f6 := Float2 (-55) (-39).
Definition i3 := makepairF f6 f5.
Notation p3 := (BND _Z i3). (* BND(Z, [-1.00044e-10, 1.00044e-10]) *)
Definition f7 := Float2 (1) (-41).
Definition f8 := Float2 (-1) (-41).
Definition i4 := makepairF f8 f7.
Notation r72 := ((_S - _S0)%R).
Notation p4 := (BND r72 i4). (* BND(S - S0, [-4.54747e-13, 4.54747e-13]) *)
Definition f9 := Float2 (1) (-34).
Definition f10 := Float2 (-1) (-34).
Definition i5 := makepairF f10 f9.
Notation r73 := ((_R - _R0)%R).
Notation p5 := (BND r73 i5). (* BND(R - R0, [-5.82077e-11, 5.82077e-11]) *)
Definition f11 := Float2 (154166255364809243) (-81).
Definition f12 := Float2 (-75807082762648785) (-80).
Definition i6 := makepairF f12 f11.
Notation p6 := (BND r70 i6). (* BND(e - E0, [-6.27061e-08, 6.37617e-08]) *)
Notation p7 := (r70 = r1). (* EQL(e - E0, e - E + (Er - E0)) *)
Lemma t1 : p7.
 apply b1 ; finalize.
Qed.
Lemma l3 : p1 -> p2 -> p3 -> p4 -> p5 -> p7. (* EQL(e - E0, e - E + (Er - E0)) *)
 intros h0 h1 h2 h3 h4.
 apply t1.
Qed.
Notation p8 := (BND r1 i6). (* BND(e - E + (Er - E0), [-6.27061e-08, 6.37617e-08]) *)
Definition f13 := Float2 (307541936830139539) (-82).
Definition f14 := Float2 (-302437757151116193) (-82).
Definition i7 := makepairF f14 f13.
Notation p9 := (BND r40 i7). (* BND(e - E, [-6.25427e-08, 6.35982e-08]) *)
Notation r75 := ((r51 - _E)%R).
Notation r76 := ((_e - r51)%R).
Notation r74 := ((r76 + r75)%R).
Notation p10 := (BND r74 i7). (* BND(e - (s1 + float<24,-149,ne>(s2 + float<24,-149,ne>(s * p))) + (s1 + float<24,-149,ne>(s2 + float<24,-149,ne>(s * p)) - E), [-6.25427e-08, 6.35982e-08]) *)
Definition f15 := Float2 (1) (-24).
Definition f16 := Float2 (-1) (-24).
Definition i8 := makepairF f16 f15.
Notation p11 := (BND r76 i8). (* BND(e - (s1 + float<24,-149,ne>(s2 + float<24,-149,ne>(s * p))), [-5.96046e-08, 5.96046e-08]) *)
Definition f17 := Float2 (561160643) (-29).
Definition f18 := Float2 (4385027001) (-32).
Definition i9 := makepairF f18 f17.
Notation p12 := (ABS r51 i9). (* ABS(s1 + float<24,-149,ne>(s2 + float<24,-149,ne>(s * p)), [1.02097, 1.04524]) *)
Notation p13 := (BND r51 i9). (* BND(s1 + float<24,-149,ne>(s2 + float<24,-149,ne>(s * p)), [1.02097, 1.04524]) *)
Definition f19 := Float2 (66971) (-16).
Definition i10 := makepairF f19 f19.
Notation p14 := (BND _s1 i10). (* BND(s1, [1.0219, 1.0219]) *)
Lemma t2 : p14.
 apply constant2 ; finalize.
Qed.
Lemma l10 : p1 -> p2 -> p3 -> p4 -> p5 -> p14. (* BND(s1, [1.0219, 1.0219]) *)
 intros h0 h1 h2 h3 h4.
 apply t2.
Qed.
Definition f20 := Float2 (12534211) (-29).
Definition f21 := Float2 (-3984455) (-32).
Definition i11 := makepairF f21 f20.
Notation p15 := (BND r52 i11). (* BND(float<24,-149,ne>(s2 + float<24,-149,ne>(s * p)), [-0.000927703, 0.0233468]) *)
Definition f22 := Float2 (410721031557) (-44).
Definition f23 := Float2 (-16320327291) (-44).
Definition i12 := makepairF f23 f22.
Notation p16 := (BND r53 i12). (* BND(s2 + float<24,-149,ne>(s * p), [-0.000927703, 0.0233468]) *)
Definition f24 := Float2 (13833605) (-44).
Definition i13 := makepairF f24 f24.
Notation p17 := (BND _s2 i13). (* BND(s2, [7.86349e-07, 7.86349e-07]) *)
Lemma t3 : p17.
 apply constant2 ; finalize.
Qed.
Lemma l13 : p1 -> p2 -> p3 -> p4 -> p5 -> p17. (* BND(s2, [7.86349e-07, 7.86349e-07]) *)
 intros h0 h1 h2 h3 h4.
 apply t3.
Qed.
Definition f25 := Float2 (12533789) (-29).
Definition f26 := Float2 (-15951329) (-34).
Definition i14 := makepairF f26 f25.
Notation p18 := (BND r54 i14). (* BND(float<24,-149,ne>(s * p), [-0.00092849, 0.023346]) *)
Definition f27 := Float2 (105141041218425) (-52).
Definition f28 := Float2 (-33452361134395) (-55).
Definition i15 := makepairF f28 f27.
Notation p19 := (BND r55 i15). (* BND(s * p, [-0.00092849, 0.023346]) *)
Definition f29 := Float2 (8572295) (-23).
Definition i16 := makepairF f29 f29.
Notation p20 := (BND _s i16). (* BND(s, [1.0219, 1.0219]) *)
Definition f30 := Float2 (17977404757381) (-44).
Definition i17 := makepairF f30 f30.
Notation p21 := (BND _S i17). (* BND(S, [1.0219, 1.0219]) *)
Lemma t4 : p14 -> p17 -> p21.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l17 : p1 -> p2 -> p3 -> p4 -> p5 -> p21. (* BND(S, [1.0219, 1.0219]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p14). apply l10. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p17). apply l13. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t4. exact h5. exact h6.
Qed.
Lemma t5 : p21 -> p20.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l16 : p1 -> p2 -> p3 -> p4 -> p5 -> p20. (* BND(s, [1.0219, 1.0219]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p21). apply l17. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t5. exact h5.
Qed.
Definition f31 := Float2 (12265215) (-29).
Definition f32 := Float2 (-3902381) (-32).
Definition i18 := makepairF f32 f31.
Notation p22 := (BND _p i18). (* BND(p, [-0.000908594, 0.0228457]) *)
Definition f33 := Float2 (842859181105641581) (-65).
Definition i19 := makepairF f32 f33.
Notation p23 := (BND r57 i19). (* BND(r1 + float<24,-149,ne>(r2 + q), [-0.000908594, 0.0228457]) *)
Definition f34 := Float2 (817349291570526317) (-65).
Definition f35 := Float2 (-3902381) (-33).
Definition i20 := makepairF f35 f34.
Notation p24 := (BND _r1 i20). (* BND(r1, [-0.000454297, 0.0221543]) *)
Notation p25 := (_r1 = r71). (* EQL(r1, R - r2) *)
Lemma t6 : p25.
 apply b2 ; finalize.
Qed.
Lemma l21 : p1 -> p2 -> p3 -> p4 -> p5 -> p25. (* EQL(r1, R - r2) *)
 intros h0 h1 h2 h3 h4.
 apply t6.
Qed.
Notation p26 := (BND r71 i20). (* BND(R - r2, [-0.000454297, 0.0221543]) *)
Definition f36 := Float2 (3902381) (-33).
Definition i21 := makepairF f35 f36.
Notation p27 := (BND _r2 i21). (* BND(r2, [-0.000454297, 0.000454297]) *)
Definition f37 := Float2 (999009561) (-41).
Definition f38 := Float2 (-999009561) (-41).
Definition i22 := makepairF f38 f37.
Notation p28 := (BND r26 i22). (* BND(-n * l2, [-0.000454297, 0.000454297]) *)
Notation p29 := (BND r28 i1). (* BND(-n, [-10176, 10176]) *)
Lemma t7 : p1 -> p29.
 intros h0.
 apply neg with (1 := h0) ; finalize.
Qed.
Lemma l25 : p1 -> p2 -> p3 -> p4 -> p5 -> p29. (* BND(-n, [-10176, 10176]) *)
 intros h0 h1 h2 h3 h4.
 apply t7. exact h0.
Qed.
Definition f39 := Float2 (6283079) (-47).
Definition i23 := makepairF f39 f39.
Notation p30 := (BND _l2 i23). (* BND(l2, [4.4644e-08, 4.4644e-08]) *)
Lemma t8 : p30.
 apply constant2 ; finalize.
Qed.
Lemma l26 : p1 -> p2 -> p3 -> p4 -> p5 -> p30. (* BND(l2, [4.4644e-08, 4.4644e-08]) *)
 intros h0 h1 h2 h3 h4.
 apply t8.
Qed.
Lemma t9 : p29 -> p30 -> p28.
 intros h0 h1.
 apply mul_op with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l24 : p1 -> p2 -> p3 -> p4 -> p5 -> p28. (* BND(-n * l2, [-0.000454297, 0.000454297]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p29). apply l25. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p30). apply l26. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t9. exact h5. exact h6.
Qed.
Lemma t10 : p28 -> p27.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l23 : p1 -> p2 -> p3 -> p4 -> p5 -> p27. (* BND(r2, [-0.000454297, 0.000454297]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p28). apply l24. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t10. exact h5.
Qed.
Lemma t11 : p2 -> p27 -> p26.
 intros h0 h1.
 apply sub with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l22 : p1 -> p2 -> p3 -> p4 -> p5 -> p26. (* BND(R - r2, [-0.000454297, 0.0221543]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p27). apply l23. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t11. exact h1. exact h5.
Qed.
Lemma t12 : p25 -> p26 -> p24.
 intros h0 h1.
 apply bnd_rewrite with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l20 : p1 -> p2 -> p3 -> p4 -> p5 -> p24. (* BND(r1, [-0.000454297, 0.0221543]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p25). apply l21. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p26). apply l22. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t12. exact h5. exact h6.
Qed.
Definition f40 := Float2 (1484871) (-31).
Definition i24 := makepairF f35 f40.
Notation p31 := (BND r58 i24). (* BND(float<24,-149,ne>(r2 + q), [-0.000454297, 0.000691447]) *)
Definition f41 := Float2 (47515871) (-36).
Definition i25 := makepairF f35 f41.
Notation p32 := (BND r59 i25). (* BND(r2 + q, [-0.000454297, 0.000691447]) *)
Definition f42 := Float2 (16296823) (-36).
Definition i26 := makepairF f4 f42.
Notation p33 := (BND _q i26). (* BND(q, [0, 0.00023715]) *)
Definition f43 := Float2 (34176914510233) (-57).
Definition i27 := makepairF f4 f43.
Notation p34 := (BND r61 i27). (* BND(float<24,-149,ne>(r * r) * float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)), [0, 0.00023715]) *)
Definition f44 := Float2 (8089829) (-34).
Definition i28 := makepairF f4 f44.
Notation p35 := (BND r67 i28). (* BND(float<24,-149,ne>(r * r), [0, 0.00047089]) *)
Definition f45 := Float2 (135724806709801) (-58).
Definition i29 := makepairF f4 f45.
Notation p36 := (BND r68 i29). (* BND(r * r, [0, 0.00047089]) *)
Definition f46 := Float2 (11650099) (-29).
Definition i30 := makepairF f4 f46.
Notation p37 := (BND _r i30). (* BND(r, [0, 0.0217]) *)
Lemma t13 : p2 -> p37.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l33 : p1 -> p2 -> p3 -> p4 -> p5 -> p37. (* BND(r, [0, 0.0217]) *)
 intros h0 h1 h2 h3 h4.
 apply t13. exact h1.
Qed.
Lemma t14 : p37 -> p36.
 intros h0.
 apply square_p with (1 := h0) ; finalize.
Qed.
Lemma l32 : p1 -> p2 -> p3 -> p4 -> p5 -> p36. (* BND(r * r, [0, 0.00047089]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p37). apply l33. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t14. exact h5.
Qed.
Lemma t15 : p36 -> p35.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l31 : p1 -> p2 -> p3 -> p4 -> p5 -> p35. (* BND(float<24,-149,ne>(r * r), [0, 0.00047089]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p36). apply l32. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t15. exact h5.
Qed.
Definition f47 := Float2 (4224677) (-23).
Definition f48 := Float2 (2097169) (-22).
Definition i31 := makepairF f48 f47.
Notation p38 := (BND r62 i31). (* BND(float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)), [0.500004, 0.503621]) *)
Definition f49 := Float2 (540758653) (-30).
Definition i32 := makepairF f48 f49.
Notation p39 := (BND r63 i32). (* BND(a1 + float<24,-149,ne>(r * a2), [0.500004, 0.503621]) *)
Definition i33 := makepairF f48 f48.
Notation p40 := (BND _a1 i33). (* BND(a1, [0.500004, 0.500004]) *)
Lemma t16 : p40.
 apply constant2 ; finalize.
Qed.
Lemma l36 : p1 -> p2 -> p3 -> p4 -> p5 -> p40. (* BND(a1, [0.500004, 0.500004]) *)
 intros h0 h1 h2 h3 h4.
 apply t16.
Qed.
Definition f50 := Float2 (3883389) (-30).
Definition i34 := makepairF f4 f50.
Notation p41 := (BND r64 i34). (* BND(float<24,-149,ne>(r * a2), [0, 0.00361669]) *)
Definition f51 := Float2 (32576228175681) (-53).
Definition i35 := makepairF f4 f51.
Notation p42 := (BND r65 i35). (* BND(r * a2, [0, 0.00361669]) *)
Definition f52 := Float2 (2796219) (-24).
Definition i36 := makepairF f52 f52.
Notation p43 := (BND _a2 i36). (* BND(a2, [0.166668, 0.166668]) *)
Lemma t17 : p43.
 apply constant2 ; finalize.
Qed.
Lemma l39 : p1 -> p2 -> p3 -> p4 -> p5 -> p43. (* BND(a2, [0.166668, 0.166668]) *)
 intros h0 h1 h2 h3 h4.
 apply t17.
Qed.
Lemma t18 : p37 -> p43 -> p42.
 intros h0 h1.
 apply mul_pp with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l38 : p1 -> p2 -> p3 -> p4 -> p5 -> p42. (* BND(r * a2, [0, 0.00361669]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p37). apply l33. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p43). apply l39. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t18. exact h5. exact h6.
Qed.
Lemma t19 : p42 -> p41.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l37 : p1 -> p2 -> p3 -> p4 -> p5 -> p41. (* BND(float<24,-149,ne>(r * a2), [0, 0.00361669]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p42). apply l38. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t19. exact h5.
Qed.
Lemma t20 : p40 -> p41 -> p39.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l35 : p1 -> p2 -> p3 -> p4 -> p5 -> p39. (* BND(a1 + float<24,-149,ne>(r * a2), [0.500004, 0.503621]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p40). apply l36. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p41). apply l37. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t20. exact h5. exact h6.
Qed.
Lemma t21 : p39 -> p38.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l34 : p1 -> p2 -> p3 -> p4 -> p5 -> p38. (* BND(float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)), [0.500004, 0.503621]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p39). apply l35. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t21. exact h5.
Qed.
Lemma t22 : p35 -> p38 -> p34.
 intros h0 h1.
 apply mul_pp with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l30 : p1 -> p2 -> p3 -> p4 -> p5 -> p34. (* BND(float<24,-149,ne>(r * r) * float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)), [0, 0.00023715]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p35). apply l31. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p38). apply l34. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t22. exact h5. exact h6.
Qed.
Lemma t23 : p34 -> p33.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l29 : p1 -> p2 -> p3 -> p4 -> p5 -> p33. (* BND(q, [0, 0.00023715]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p34). apply l30. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t23. exact h5.
Qed.
Lemma t24 : p27 -> p33 -> p32.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l28 : p1 -> p2 -> p3 -> p4 -> p5 -> p32. (* BND(r2 + q, [-0.000454297, 0.000691447]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p27). apply l23. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p33). apply l29. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t24. exact h5. exact h6.
Qed.
Lemma t25 : p32 -> p31.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l27 : p1 -> p2 -> p3 -> p4 -> p5 -> p31. (* BND(float<24,-149,ne>(r2 + q), [-0.000454297, 0.000691447]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p32). apply l28. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t25. exact h5.
Qed.
Lemma t26 : p24 -> p31 -> p23.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l19 : p1 -> p2 -> p3 -> p4 -> p5 -> p23. (* BND(r1 + float<24,-149,ne>(r2 + q), [-0.000908594, 0.0228457]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p24). apply l20. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p31). apply l27. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t26. exact h5. exact h6.
Qed.
Lemma t27 : p23 -> p22.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l18 : p1 -> p2 -> p3 -> p4 -> p5 -> p22. (* BND(p, [-0.000908594, 0.0228457]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p23). apply l19. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t27. exact h5.
Qed.
Lemma t28 : p20 -> p22 -> p19.
 intros h0 h1.
 apply mul_po with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l15 : p1 -> p2 -> p3 -> p4 -> p5 -> p19. (* BND(s * p, [-0.00092849, 0.023346]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p20). apply l16. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p22). apply l18. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t28. exact h5. exact h6.
Qed.
Lemma t29 : p19 -> p18.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l14 : p1 -> p2 -> p3 -> p4 -> p5 -> p18. (* BND(float<24,-149,ne>(s * p), [-0.00092849, 0.023346]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p19). apply l15. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t29. exact h5.
Qed.
Lemma t30 : p17 -> p18 -> p16.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l12 : p1 -> p2 -> p3 -> p4 -> p5 -> p16. (* BND(s2 + float<24,-149,ne>(s * p), [-0.000927703, 0.0233468]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p17). apply l13. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p18). apply l14. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t30. exact h5. exact h6.
Qed.
Lemma t31 : p16 -> p15.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l11 : p1 -> p2 -> p3 -> p4 -> p5 -> p15. (* BND(float<24,-149,ne>(s2 + float<24,-149,ne>(s * p)), [-0.000927703, 0.0233468]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p16). apply l12. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t31. exact h5.
Qed.
Lemma t32 : p14 -> p15 -> p13.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l9 : p1 -> p2 -> p3 -> p4 -> p5 -> p13. (* BND(s1 + float<24,-149,ne>(s2 + float<24,-149,ne>(s * p)), [1.02097, 1.04524]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p14). apply l10. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p15). apply l11. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t32. exact h5. exact h6.
Qed.
Lemma t33 : p13 -> p12.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l8 : p1 -> p2 -> p3 -> p4 -> p5 -> p12. (* ABS(s1 + float<24,-149,ne>(s2 + float<24,-149,ne>(s * p)), [1.02097, 1.04524]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p13). apply l9. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t33. exact h5.
Qed.
Lemma t34 : p12 -> p11.
 intros h0.
 apply float_absolute_ne with (1 := h0) ; finalize.
Qed.
Lemma l7 : p1 -> p2 -> p3 -> p4 -> p5 -> p11. (* BND(e - (s1 + float<24,-149,ne>(s2 + float<24,-149,ne>(s * p))), [-5.96046e-08, 5.96046e-08]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p12). apply l8. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t34. exact h5.
Qed.
Definition f53 := Float2 (617969941709689427) (-87).
Definition f54 := Float2 (-454636191980942353) (-87).
Definition i37 := makepairF f54 f53.
Notation p44 := (BND r75 i37). (* BND(s1 + float<24,-149,ne>(s2 + float<24,-149,ne>(s * p)) - E, [-2.93802e-09, 3.99354e-09]) *)
Notation r77 := ((r52 - r42)%R).
Notation p45 := (BND r77 i37). (* BND(float<24,-149,ne>(s2 + float<24,-149,ne>(s * p)) - (s2 + S * (r1 + (r2 + R * R * (a1 + R * a2)))), [-2.93802e-09, 3.99354e-09]) *)
Notation r79 := ((r53 - r42)%R).
Notation r80 := ((r52 - r53)%R).
Notation r78 := ((r80 + r79)%R).
Notation p46 := (BND r78 i37). (* BND(float<24,-149,ne>(s2 + float<24,-149,ne>(s * p)) - (s2 + float<24,-149,ne>(s * p)) + (s2 + float<24,-149,ne>(s * p) - (s2 + S * (r1 + (r2 + R * R * (a1 + R * a2))))), [-2.93802e-09, 3.99354e-09]) *)
Definition f55 := Float2 (1) (-30).
Definition f56 := Float2 (-1) (-30).
Definition i38 := makepairF f56 f55.
Notation p47 := (BND r80 i38). (* BND(float<24,-149,ne>(s2 + float<24,-149,ne>(s * p)) - (s2 + float<24,-149,ne>(s * p)), [-9.31323e-10, 9.31323e-10]) *)
Definition i39 := makepairF f4 f22.
Notation p48 := (ABS r53 i39). (* ABS(s2 + float<24,-149,ne>(s * p), [0, 0.0233468]) *)
Notation p49 := (ABS _s2 i13). (* ABS(s2, [7.86349e-07, 7.86349e-07]) *)
Lemma t35 : p17 -> p49.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l45 : p1 -> p2 -> p3 -> p4 -> p5 -> p49. (* ABS(s2, [7.86349e-07, 7.86349e-07]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p17). apply l13. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t35. exact h5.
Qed.
Definition i40 := makepairF f4 f25.
Notation p50 := (ABS r54 i40). (* ABS(float<24,-149,ne>(s * p), [0, 0.023346]) *)
Lemma t36 : p18 -> p50.
 intros h0.
 apply abs_of_bnd_o with (1 := h0) ; finalize.
Qed.
Lemma l46 : p1 -> p2 -> p3 -> p4 -> p5 -> p50. (* ABS(float<24,-149,ne>(s * p), [0, 0.023346]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p18). apply l14. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t36. exact h5.
Qed.
Lemma t37 : p49 -> p50 -> p48.
 intros h0 h1.
 apply add_aa_o with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l44 : p1 -> p2 -> p3 -> p4 -> p5 -> p48. (* ABS(s2 + float<24,-149,ne>(s * p), [0, 0.0233468]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p49). apply l45. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p50). apply l46. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t37. exact h5. exact h6.
Qed.
Lemma t38 : p48 -> p47.
 intros h0.
 apply float_absolute_ne with (1 := h0) ; finalize.
Qed.
Lemma l43 : p1 -> p2 -> p3 -> p4 -> p5 -> p47. (* BND(float<24,-149,ne>(s2 + float<24,-149,ne>(s * p)) - (s2 + float<24,-149,ne>(s * p)), [-9.31323e-10, 9.31323e-10]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p48). apply l44. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t38. exact h5.
Qed.
Definition f57 := Float2 (947709507267667109) (-88).
Definition f58 := Float2 (-310521003905086481) (-87).
Definition i41 := makepairF f58 f57.
Notation p51 := (BND r79 i41). (* BND(s2 + float<24,-149,ne>(s * p) - (s2 + S * (r1 + (r2 + R * R * (a1 + R * a2)))), [-2.00669e-09, 3.06221e-09]) *)
Notation r81 := ((r54 - r43)%R).
Notation p52 := (BND r81 i41). (* BND(float<24,-149,ne>(s * p) - S * (r1 + (r2 + R * R * (a1 + R * a2))), [-2.00669e-09, 3.06221e-09]) *)
Notation r83 := ((r55 - r43)%R).
Notation r84 := ((r54 - r55)%R).
Notation r82 := ((r84 + r83)%R).
Notation p53 := (BND r82 i41). (* BND(float<24,-149,ne>(s * p) - s * p + (s * p - S * (r1 + (r2 + R * R * (a1 + R * a2)))), [-2.00669e-09, 3.06221e-09]) *)
Notation p54 := (BND r84 i38). (* BND(float<24,-149,ne>(s * p) - s * p, [-9.31323e-10, 9.31323e-10]) *)
Definition i42 := makepairF f4 f27.
Notation p55 := (ABS r55 i42). (* ABS(s * p, [0, 0.023346]) *)
Notation p56 := (ABS _s i16). (* ABS(s, [1.0219, 1.0219]) *)
Lemma t39 : p20 -> p56.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l52 : p1 -> p2 -> p3 -> p4 -> p5 -> p56. (* ABS(s, [1.0219, 1.0219]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p20). apply l16. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t39. exact h5.
Qed.
Definition i43 := makepairF f4 f31.
Notation p57 := (ABS _p i43). (* ABS(p, [0, 0.0228457]) *)
Lemma t40 : p22 -> p57.
 intros h0.
 apply abs_of_bnd_o with (1 := h0) ; finalize.
Qed.
Lemma l53 : p1 -> p2 -> p3 -> p4 -> p5 -> p57. (* ABS(p, [0, 0.0228457]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p22). apply l18. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t40. exact h5.
Qed.
Lemma t41 : p56 -> p57 -> p55.
 intros h0 h1.
 apply mul_aa with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l51 : p1 -> p2 -> p3 -> p4 -> p5 -> p55. (* ABS(s * p, [0, 0.023346]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p56). apply l52. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p57). apply l53. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t41. exact h5. exact h6.
Qed.
Lemma t42 : p55 -> p54.
 intros h0.
 apply float_absolute_ne with (1 := h0) ; finalize.
Qed.
Lemma l50 : p1 -> p2 -> p3 -> p4 -> p5 -> p54. (* BND(float<24,-149,ne>(s * p) - s * p, [-9.31323e-10, 9.31323e-10]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p55). apply l51. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t42. exact h5.
Qed.
Definition f59 := Float2 (659479131115955365) (-88).
Definition f60 := Float2 (-166405815829230609) (-87).
Definition i44 := makepairF f60 f59.
Notation p58 := (BND r83 i44). (* BND(s * p - S * (r1 + (r2 + R * R * (a1 + R * a2))), [-1.07537e-09, 2.13089e-09]) *)
Notation r87 := ((_p - r44)%R).
Notation r86 := ((_S * r87)%R).
Notation r89 := ((_s - _S)%R).
Notation r88 := ((r89 * _p)%R).
Notation r85 := ((r88 + r86)%R).
Notation p59 := (BND r85 i44). (* BND((s - S) * p + S * (p - (r1 + (r2 + R * R * (a1 + R * a2)))), [-1.07537e-09, 2.13089e-09]) *)
Definition f61 := Float2 (10382001623685) (-73).
Definition f62 := Float2 (-3303205518879) (-76).
Definition i45 := makepairF f62 f61.
Notation p60 := (BND r88 i45). (* BND((s - S) * p, [-4.37176e-11, 1.09924e-09]) *)
Definition f63 := Float2 (846459) (-44).
Definition i46 := makepairF f63 f63.
Notation p61 := (BND r89 i46). (* BND(s - S, [4.81156e-08, 4.81156e-08]) *)
Lemma t43 : p20 -> p21 -> p61.
 intros h0 h1.
 apply sub with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l57 : p1 -> p2 -> p3 -> p4 -> p5 -> p61. (* BND(s - S, [4.81156e-08, 4.81156e-08]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p20). apply l16. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p21). apply l17. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t43. exact h5. exact h6.
Qed.
Lemma t44 : p61 -> p22 -> p60.
 intros h0 h1.
 apply mul_po with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l56 : p1 -> p2 -> p3 -> p4 -> p5 -> p60. (* BND((s - S) * p, [-4.37176e-11, 1.09924e-09]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p61). apply l57. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p22). apply l18. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t44. exact h5. exact h6.
Qed.
Definition f64 := Float2 (319281701911045285) (-88).
Definition f65 := Float2 (-159640850926566417) (-87).
Definition i47 := makepairF f65 f64.
Notation p62 := (BND r86 i47). (* BND(S * (p - (r1 + (r2 + R * R * (a1 + R * a2)))), [-1.03165e-09, 1.03165e-09]) *)
Definition f66 := Float2 (39055019178246233) (-85).
Definition f67 := Float2 (-312440153369298357) (-88).
Definition i48 := makepairF f67 f66.
Notation p63 := (BND r87 i48). (* BND(p - (r1 + (r2 + R * R * (a1 + R * a2))), [-1.00955e-09, 1.00955e-09]) *)
Notation r91 := ((r57 - r44)%R).
Notation r92 := ((_p - r57)%R).
Notation r90 := ((r92 + r91)%R).
Notation p64 := (BND r90 i48). (* BND(p - (r1 + float<24,-149,ne>(r2 + q)) + (r1 + float<24,-149,ne>(r2 + q) - (r1 + (r2 + R * R * (a1 + R * a2)))), [-1.00955e-09, 1.00955e-09]) *)
Notation p65 := (BND r92 i38). (* BND(p - (r1 + float<24,-149,ne>(r2 + q)), [-9.31323e-10, 9.31323e-10]) *)
Definition i49 := makepairF f4 f33.
Notation p66 := (ABS r57 i49). (* ABS(r1 + float<24,-149,ne>(r2 + q), [0, 0.0228457]) *)
Definition i50 := makepairF f4 f34.
Notation p67 := (ABS _r1 i50). (* ABS(r1, [0, 0.0221543]) *)
Lemma t45 : p24 -> p67.
 intros h0.
 apply abs_of_bnd_o with (1 := h0) ; finalize.
Qed.
Lemma l63 : p1 -> p2 -> p3 -> p4 -> p5 -> p67. (* ABS(r1, [0, 0.0221543]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p24). apply l20. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t45. exact h5.
Qed.
Definition i51 := makepairF f4 f40.
Notation p68 := (ABS r58 i51). (* ABS(float<24,-149,ne>(r2 + q), [0, 0.000691447]) *)
Lemma t46 : p31 -> p68.
 intros h0.
 apply abs_of_bnd_o with (1 := h0) ; finalize.
Qed.
Lemma l64 : p1 -> p2 -> p3 -> p4 -> p5 -> p68. (* ABS(float<24,-149,ne>(r2 + q), [0, 0.000691447]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p31). apply l27. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t46. exact h5.
Qed.
Lemma t47 : p67 -> p68 -> p66.
 intros h0 h1.
 apply add_aa_o with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l62 : p1 -> p2 -> p3 -> p4 -> p5 -> p66. (* ABS(r1 + float<24,-149,ne>(r2 + q), [0, 0.0228457]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p67). apply l63. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p68). apply l64. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t47. exact h5. exact h6.
Qed.
Lemma t48 : p66 -> p65.
 intros h0.
 apply float_absolute_ne with (1 := h0) ; finalize.
Qed.
Lemma l61 : p1 -> p2 -> p3 -> p4 -> p5 -> p65. (* BND(p - (r1 + float<24,-149,ne>(r2 + q)), [-9.31323e-10, 9.31323e-10]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p66). apply l62. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t48. exact h5.
Qed.
Definition f68 := Float2 (387356436388129919) (-92).
Definition f69 := Float2 (-774712870962771609) (-93).
Definition i52 := makepairF f69 f68.
Notation p69 := (BND r91 i52). (* BND(r1 + float<24,-149,ne>(r2 + q) - (r1 + (r2 + R * R * (a1 + R * a2))), [-7.8226e-11, 7.8226e-11]) *)
Notation r93 := ((r58 - r45)%R).
Notation p70 := (BND r93 i52). (* BND(float<24,-149,ne>(r2 + q) - (r2 + R * R * (a1 + R * a2)), [-7.8226e-11, 7.8226e-11]) *)
Notation r95 := ((r59 - r45)%R).
Notation r96 := ((r58 - r59)%R).
Notation r94 := ((r96 + r95)%R).
Notation p71 := (BND r94 i52). (* BND(float<24,-149,ne>(r2 + q) - (r2 + q) + (r2 + q - (r2 + R * R * (a1 + R * a2))), [-7.8226e-11, 7.8226e-11]) *)
Definition f70 := Float2 (1) (-35).
Definition f71 := Float2 (-1) (-35).
Definition i53 := makepairF f71 f70.
Notation p72 := (BND r96 i53). (* BND(float<24,-149,ne>(r2 + q) - (r2 + q), [-2.91038e-11, 2.91038e-11]) *)
Definition i54 := makepairF f4 f41.
Notation p73 := (ABS r59 i54). (* ABS(r2 + q, [0, 0.000691447]) *)
Definition i55 := makepairF f4 f36.
Notation p74 := (ABS _r2 i55). (* ABS(r2, [0, 0.000454297]) *)
Lemma t49 : p27 -> p74.
 intros h0.
 apply abs_of_bnd_o with (1 := h0) ; finalize.
Qed.
Lemma l70 : p1 -> p2 -> p3 -> p4 -> p5 -> p74. (* ABS(r2, [0, 0.000454297]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p27). apply l23. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t49. exact h5.
Qed.
Notation p75 := (ABS _q i26). (* ABS(q, [0, 0.00023715]) *)
Lemma t50 : p33 -> p75.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l71 : p1 -> p2 -> p3 -> p4 -> p5 -> p75. (* ABS(q, [0, 0.00023715]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p33). apply l29. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t50. exact h5.
Qed.
Lemma t51 : p74 -> p75 -> p73.
 intros h0 h1.
 apply add_aa_o with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l69 : p1 -> p2 -> p3 -> p4 -> p5 -> p73. (* ABS(r2 + q, [0, 0.000691447]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p74). apply l70. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p75). apply l71. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t51. exact h5. exact h6.
Qed.
Lemma t52 : p73 -> p72.
 intros h0.
 apply float_absolute_ne with (1 := h0) ; finalize.
Qed.
Lemma l68 : p1 -> p2 -> p3 -> p4 -> p5 -> p72. (* BND(float<24,-149,ne>(r2 + q) - (r2 + q), [-2.91038e-11, 2.91038e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p73). apply l69. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t52. exact h5.
Qed.
Definition f72 := Float2 (972964993249096187) (-94).
Definition f73 := Float2 (-486482494811059865) (-93).
Definition i56 := makepairF f73 f72.
Notation p76 := (BND r95 i56). (* BND(r2 + q - (r2 + R * R * (a1 + R * a2)), [-4.91222e-11, 4.91222e-11]) *)
Notation r97 := ((_q - r46)%R).
Notation p77 := (BND r97 i56). (* BND(q - R * R * (a1 + R * a2), [-4.91222e-11, 4.91222e-11]) *)
Notation r99 := ((r61 - r46)%R).
Notation r100 := ((_q - r61)%R).
Notation r98 := ((r100 + r99)%R).
Notation p78 := (BND r98 i56). (* BND(q - float<24,-149,ne>(r * r) * float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) + (float<24,-149,ne>(r * r) * float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) - R * R * (a1 + R * a2)), [-4.91222e-11, 4.91222e-11]) *)
Definition f74 := Float2 (1) (-37).
Definition f75 := Float2 (-1) (-37).
Definition i57 := makepairF f75 f74.
Notation p79 := (BND r100 i57). (* BND(q - float<24,-149,ne>(r * r) * float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)), [-7.27596e-12, 7.27596e-12]) *)
Notation p80 := (ABS r61 i27). (* ABS(float<24,-149,ne>(r * r) * float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)), [0, 0.00023715]) *)
Notation p81 := (ABS r67 i28). (* ABS(float<24,-149,ne>(r * r), [0, 0.00047089]) *)
Lemma t53 : p35 -> p81.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l77 : p1 -> p2 -> p3 -> p4 -> p5 -> p81. (* ABS(float<24,-149,ne>(r * r), [0, 0.00047089]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p35). apply l31. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t53. exact h5.
Qed.
Notation p82 := (ABS r62 i31). (* ABS(float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)), [0.500004, 0.503621]) *)
Lemma t54 : p38 -> p82.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l78 : p1 -> p2 -> p3 -> p4 -> p5 -> p82. (* ABS(float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)), [0.500004, 0.503621]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p38). apply l34. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t54. exact h5.
Qed.
Lemma t55 : p81 -> p82 -> p80.
 intros h0 h1.
 apply mul_aa with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l76 : p1 -> p2 -> p3 -> p4 -> p5 -> p80. (* ABS(float<24,-149,ne>(r * r) * float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)), [0, 0.00023715]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p81). apply l77. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p82). apply l78. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t55. exact h5. exact h6.
Qed.
Lemma t56 : p80 -> p79.
 intros h0.
 apply float_absolute_ne with (1 := h0) ; finalize.
Qed.
Lemma l75 : p1 -> p2 -> p3 -> p4 -> p5 -> p79. (* BND(q - float<24,-149,ne>(r * r) * float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)), [-7.27596e-12, 7.27596e-12]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p80). apply l76. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t56. exact h5.
Qed.
Definition f76 := Float2 (828849805173240315) (-94).
Definition f77 := Float2 (-414424900773131929) (-93).
Definition i58 := makepairF f77 f76.
Notation p83 := (BND r99 i58). (* BND(float<24,-149,ne>(r * r) * float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) - R * R * (a1 + R * a2), [-4.18462e-11, 4.18462e-11]) *)
Notation r103 := ((r62 - r47)%R).
Notation r102 := ((r49 * r103)%R).
Notation r105 := ((r67 - r49)%R).
Notation r104 := ((r105 * r62)%R).
Notation r101 := ((r104 + r102)%R).
Notation p84 := (BND r101 i58). (* BND((float<24,-149,ne>(r * r) - R * R) * float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) + R * R * (float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) - (a1 + R * a2)), [-4.18462e-11, 4.18462e-11]) *)
Definition f78 := Float2 (548351872942775095) (-94).
Definition f79 := Float2 (-1096703738631597275) (-95).
Definition i59 := makepairF f79 f78.
Notation p85 := (BND r104 i59). (* BND((float<24,-149,ne>(r * r) - R * R) * float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)), [-2.76847e-11, 2.76847e-11]) *)
Definition f80 := Float2 (1088819076152507447) (-94).
Definition f81 := Float2 (-1088819068950706285) (-94).
Definition i60 := makepairF f81 f80.
Notation p86 := (BND r105 i60). (* BND(float<24,-149,ne>(r * r) - R * R, [-5.49713e-11, 5.49713e-11]) *)
Notation r107 := ((r68 - r49)%R).
Notation r108 := ((r67 - r68)%R).
Notation r106 := ((r108 + r107)%R).
Notation p87 := (BND r106 i60). (* BND(float<24,-149,ne>(r * r) - r * r + (r * r - R * R), [-5.49713e-11, 5.49713e-11]) *)
Definition f82 := Float2 (1) (-36).
Definition f83 := Float2 (-1) (-36).
Definition i61 := makepairF f83 f82.
Notation p88 := (BND r108 i61). (* BND(float<24,-149,ne>(r * r) - r * r, [-1.45519e-11, 1.45519e-11]) *)
Notation p89 := (ABS r68 i29). (* ABS(r * r, [0, 0.00047089]) *)
Notation p90 := (ABS _r i30). (* ABS(r, [0, 0.0217]) *)
Lemma t57 : p37 -> p90.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l86 : p1 -> p2 -> p3 -> p4 -> p5 -> p90. (* ABS(r, [0, 0.0217]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p37). apply l33. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t57. exact h5.
Qed.
Lemma t58 : p90 -> p90 -> p89.
 intros h0 h1.
 apply mul_aa with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l85 : p1 -> p2 -> p3 -> p4 -> p5 -> p89. (* ABS(r * r, [0, 0.00047089]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p90). apply l86. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t58. exact h5. exact h5.
Qed.
Lemma t59 : p89 -> p88.
 intros h0.
 apply float_absolute_ne with (1 := h0) ; finalize.
Qed.
Lemma l84 : p1 -> p2 -> p3 -> p4 -> p5 -> p88. (* BND(float<24,-149,ne>(r * r) - r * r, [-1.45519e-11, 1.45519e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p89). apply l85. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t59. exact h5.
Qed.
Definition f84 := Float2 (800588700000795703) (-94).
Definition f85 := Float2 (-800588692798994541) (-94).
Definition i62 := makepairF f85 f84.
Notation p91 := (BND r107 i62). (* BND(r * r - R * R, [-4.04194e-11, 4.04194e-11]) *)
Definition i63 := makepairF f10 f84.
Notation p92 := (BND r107 i63). (* BND(r * r - R * R, [-5.82077e-11, 4.04194e-11]) *)
Notation r111 := ((_r - _R)%R).
Notation r110 := ((_R * r111)%R).
Notation r112 := ((r111 * _r)%R).
Notation r109 := ((r112 + r110)%R).
Notation p93 := (BND r109 i63). (* BND((r - R) * r + R * (r - R), [-5.82077e-11, 4.04194e-11]) *)
Definition f86 := Float2 (11650099) (-59).
Definition i64 := makepairF f71 f86.
Notation p94 := (BND r112 i64). (* BND((r - R) * r, [-2.91038e-11, 2.02097e-11]) *)
Notation p95 := (BND r111 i38). (* BND(r - R, [-9.31323e-10, 9.31323e-10]) *)
Notation p96 := (ABS _R i2). (* ABS(R, [0, 0.0217]) *)
Lemma t60 : p2 -> p96.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l92 : p1 -> p2 -> p3 -> p4 -> p5 -> p96. (* ABS(R, [0, 0.0217]) *)
 intros h0 h1 h2 h3 h4.
 apply t60. exact h1.
Qed.
Lemma t61 : p96 -> p95.
 intros h0.
 apply float_absolute_ne with (1 := h0) ; finalize.
Qed.
Lemma l91 : p1 -> p2 -> p3 -> p4 -> p5 -> p95. (* BND(r - R, [-9.31323e-10, 9.31323e-10]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p96). apply l92. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t61. exact h5.
Qed.
Lemma t62 : p95 -> p37 -> p94.
 intros h0 h1.
 apply mul_op with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l90 : p1 -> p2 -> p3 -> p4 -> p5 -> p94. (* BND((r - R) * r, [-2.91038e-11, 2.02097e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p95). apply l91. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p37). apply l33. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t62. exact h5. exact h6.
Qed.
Definition f87 := Float2 (400294346399497271) (-94).
Definition f88 := Float2 (-800588692798994541) (-95).
Definition i65 := makepairF f88 f87.
Notation p97 := (BND r110 i65). (* BND(R * (r - R), [-2.02097e-11, 2.02097e-11]) *)
Lemma t63 : p2 -> p95 -> p97.
 intros h0 h1.
 apply mul_po with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l93 : p1 -> p2 -> p3 -> p4 -> p5 -> p97. (* BND(R * (r - R), [-2.02097e-11, 2.02097e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p95). apply l91. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t63. exact h1. exact h5.
Qed.
Definition i66 := makepairF f71 f87.
Notation p98 := (BND r110 i66). (* BND(R * (r - R), [-2.91038e-11, 2.02097e-11]) *)
Lemma t64 : p94 -> p98 -> p93.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l89 : p1 -> p2 -> p3 -> p4 -> p5 -> p93. (* BND((r - R) * r + R * (r - R), [-5.82077e-11, 4.04194e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p94). apply l90. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p97). apply l93. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t64. exact h5. apply subset with (1 := h6). finalize.
Qed.
Lemma t65 : p93 -> p92.
 intros h0.
 apply mul_mals with (1 := h0) ; finalize.
Qed.
Lemma l88 : p1 -> p2 -> p3 -> p4 -> p5 -> p92. (* BND(r * r - R * R, [-5.82077e-11, 4.04194e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p93). apply l89. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t65. exact h5.
Qed.
Definition i67 := makepairF f85 f9.
Notation p99 := (BND r107 i67). (* BND(r * r - R * R, [-4.04194e-11, 5.82077e-11]) *)
Notation r114 := ((r111 * r111)%R).
Notation r116 := ((r111 * _R)%R).
Notation r115 := ((r110 + r116)%R).
Notation r113 := ((r115 + r114)%R).
Notation p100 := (BND r113 i67). (* BND(R * (r - R) + (r - R) * R + (r - R) * (r - R), [-4.04194e-11, 5.82077e-11]) *)
Definition f89 := Float2 (3) (-36).
Definition i68 := makepairF f85 f89.
Notation p101 := (BND r115 i68). (* BND(R * (r - R) + (r - R) * R, [-4.04194e-11, 4.36557e-11]) *)
Definition f90 := Float2 (3) (-37).
Definition i69 := makepairF f88 f90.
Notation p102 := (BND r116 i69). (* BND((r - R) * R, [-2.02097e-11, 2.18279e-11]) *)
Lemma t66 : p95 -> p2 -> p102.
 intros h0 h1.
 apply mul_op with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l97 : p1 -> p2 -> p3 -> p4 -> p5 -> p102. (* BND((r - R) * R, [-2.02097e-11, 2.18279e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p95). apply l91. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t66. exact h5. exact h1.
Qed.
Notation p103 := (BND r110 i69). (* BND(R * (r - R), [-2.02097e-11, 2.18279e-11]) *)
Lemma t67 : p103 -> p102 -> p101.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l96 : p1 -> p2 -> p3 -> p4 -> p5 -> p101. (* BND(R * (r - R) + (r - R) * R, [-4.04194e-11, 4.36557e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p97). apply l93. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p102). apply l97. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t67. apply subset with (1 := h5). finalize. exact h6.
Qed.
Definition i70 := makepairF f4 f82.
Notation p104 := (BND r114 i70). (* BND((r - R) * (r - R), [0, 1.45519e-11]) *)
Definition f91 := Float2 (1) (-18).
Definition f92 := Float2 (-1) (-18).
Definition i71 := makepairF f92 f91.
Notation p105 := (BND r111 i71). (* BND(r - R, [-3.8147e-06, 3.8147e-06]) *)
Lemma t68 : p105 -> p104.
 intros h0.
 apply square_o with (1 := h0) ; finalize.
Qed.
Lemma l98 : p1 -> p2 -> p3 -> p4 -> p5 -> p104. (* BND((r - R) * (r - R), [0, 1.45519e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p95). apply l91. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t68. apply subset with (1 := h5). finalize.
Qed.
Lemma t69 : p101 -> p104 -> p100.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l95 : p1 -> p2 -> p3 -> p4 -> p5 -> p100. (* BND(R * (r - R) + (r - R) * R + (r - R) * (r - R), [-4.04194e-11, 5.82077e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p101). apply l96. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p104). apply l98. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t69. exact h5. exact h6.
Qed.
Lemma t70 : p100 -> p99.
 intros h0.
 apply mul_mibs with (1 := h0) ; finalize.
Qed.
Lemma l94 : p1 -> p2 -> p3 -> p4 -> p5 -> p99. (* BND(r * r - R * R, [-4.04194e-11, 5.82077e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p100). apply l95. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t70. exact h5.
Qed.
Lemma l87 : p1 -> p2 -> p3 -> p4 -> p5 -> p91. (* BND(r * r - R * R, [-4.04194e-11, 4.04194e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p92). apply l88. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p99). apply l94. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply intersect with (1 := h5) (2 := h6). finalize.
Qed.
Lemma t71 : p88 -> p91 -> p87.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l83 : p1 -> p2 -> p3 -> p4 -> p5 -> p87. (* BND(float<24,-149,ne>(r * r) - r * r + (r * r - R * R), [-5.49713e-11, 5.49713e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p88). apply l84. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p91). apply l87. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t71. exact h5. exact h6.
Qed.
Lemma t72 : p87 -> p86.
 intros h0.
 apply sub_xals with (1 := h0) ; finalize.
Qed.
Lemma l82 : p1 -> p2 -> p3 -> p4 -> p5 -> p86. (* BND(float<24,-149,ne>(r * r) - R * R, [-5.49713e-11, 5.49713e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p87). apply l83. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t72. exact h5.
Qed.
Lemma t73 : p86 -> p38 -> p85.
 intros h0 h1.
 apply mul_op with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l81 : p1 -> p2 -> p3 -> p4 -> p5 -> p85. (* BND((float<24,-149,ne>(r * r) - R * R) * float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)), [-2.76847e-11, 2.76847e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p86). apply l82. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p38). apply l34. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t73. exact h5. exact h6.
Qed.
Definition f93 := Float2 (1121991728921860879) (-96).
Definition f94 := Float2 (-1121991728921860879) (-96).
Definition i72 := makepairF f94 f93.
Notation p106 := (BND r102 i72). (* BND(R * R * (float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) - (a1 + R * a2)), [-1.41615e-11, 1.41615e-11]) *)
Definition f95 := Float2 (277964394139810905) (-69).
Definition i73 := makepairF f4 f95.
Notation p107 := (BND r49 i73). (* BND(R * R, [0, 0.00047089]) *)
Lemma t74 : p2 -> p107.
 intros h0.
 apply square_p with (1 := h0) ; finalize.
Qed.
Lemma l100 : p1 -> p2 -> p3 -> p4 -> p5 -> p107. (* BND(R * R, [0, 0.00047089]) *)
 intros h0 h1 h2 h3 h4.
 apply t74. exact h1.
Qed.
Definition f96 := Float2 (541764283) (-54).
Definition f97 := Float2 (-541764283) (-54).
Definition i74 := makepairF f97 f96.
Notation p108 := (BND r103 i74). (* BND(float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) - (a1 + R * a2), [-3.0074e-08, 3.0074e-08]) *)
Notation r118 := ((r63 - r47)%R).
Notation r119 := ((r62 - r63)%R).
Notation r117 := ((r119 + r118)%R).
Notation p109 := (BND r117 i74). (* BND(float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) - (a1 + float<24,-149,ne>(r * a2)) + (a1 + float<24,-149,ne>(r * a2) - (a1 + R * a2)), [-3.0074e-08, 3.0074e-08]) *)
Definition f98 := Float2 (1) (-25).
Definition f99 := Float2 (-1) (-25).
Definition i75 := makepairF f99 f98.
Notation p110 := (BND r119 i75). (* BND(float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) - (a1 + float<24,-149,ne>(r * a2)), [-2.98023e-08, 2.98023e-08]) *)
Notation p111 := (ABS r63 i32). (* ABS(a1 + float<24,-149,ne>(r * a2), [0.500004, 0.503621]) *)
Lemma t75 : p39 -> p111.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l104 : p1 -> p2 -> p3 -> p4 -> p5 -> p111. (* ABS(a1 + float<24,-149,ne>(r * a2), [0.500004, 0.503621]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p39). apply l35. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t75. exact h5.
Qed.
Lemma t76 : p111 -> p110.
 intros h0.
 apply float_absolute_ne with (1 := h0) ; finalize.
Qed.
Lemma l103 : p1 -> p2 -> p3 -> p4 -> p5 -> p110. (* BND(float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) - (a1 + float<24,-149,ne>(r * a2)), [-2.98023e-08, 2.98023e-08]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p111). apply l104. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t76. exact h5.
Qed.
Definition f100 := Float2 (4893371) (-54).
Definition f101 := Float2 (-4893371) (-54).
Definition i76 := makepairF f101 f100.
Notation p112 := (BND r118 i76). (* BND(a1 + float<24,-149,ne>(r * a2) - (a1 + R * a2), [-2.71637e-10, 2.71637e-10]) *)
Notation r120 := ((r64 - r48)%R).
Notation p113 := (BND r120 i76). (* BND(float<24,-149,ne>(r * a2) - R * a2, [-2.71637e-10, 2.71637e-10]) *)
Notation r122 := ((r65 - r48)%R).
Notation r123 := ((r64 - r65)%R).
Notation r121 := ((r123 + r122)%R).
Notation p114 := (BND r121 i76). (* BND(float<24,-149,ne>(r * a2) - r * a2 + (r * a2 - R * a2), [-2.71637e-10, 2.71637e-10]) *)
Definition f102 := Float2 (1) (-33).
Definition f103 := Float2 (-1) (-33).
Definition i77 := makepairF f103 f102.
Notation p115 := (BND r123 i77). (* BND(float<24,-149,ne>(r * a2) - r * a2, [-1.16415e-10, 1.16415e-10]) *)
Notation p116 := (ABS r65 i35). (* ABS(r * a2, [0, 0.00361669]) *)
Notation p117 := (ABS _a2 i36). (* ABS(a2, [0.166668, 0.166668]) *)
Lemma t77 : p43 -> p117.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l110 : p1 -> p2 -> p3 -> p4 -> p5 -> p117. (* ABS(a2, [0.166668, 0.166668]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p43). apply l39. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t77. exact h5.
Qed.
Lemma t78 : p90 -> p117 -> p116.
 intros h0 h1.
 apply mul_aa with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l109 : p1 -> p2 -> p3 -> p4 -> p5 -> p116. (* ABS(r * a2, [0, 0.00361669]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p90). apply l86. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p117). apply l110. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t78. exact h5. exact h6.
Qed.
Lemma t79 : p116 -> p115.
 intros h0.
 apply float_absolute_ne with (1 := h0) ; finalize.
Qed.
Lemma l108 : p1 -> p2 -> p3 -> p4 -> p5 -> p115. (* BND(float<24,-149,ne>(r * a2) - r * a2, [-1.16415e-10, 1.16415e-10]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p116). apply l109. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t79. exact h5.
Qed.
Definition f104 := Float2 (2796219) (-54).
Definition f105 := Float2 (-2796219) (-54).
Definition i78 := makepairF f105 f104.
Notation p118 := (BND r122 i78). (* BND(r * a2 - R * a2, [-1.55221e-10, 1.55221e-10]) *)
Notation r124 := ((r111 * _a2)%R).
Notation p119 := (BND r124 i78). (* BND((r - R) * a2, [-1.55221e-10, 1.55221e-10]) *)
Lemma t80 : p95 -> p43 -> p119.
 intros h0 h1.
 apply mul_op with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l112 : p1 -> p2 -> p3 -> p4 -> p5 -> p119. (* BND((r - R) * a2, [-1.55221e-10, 1.55221e-10]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p95). apply l91. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p43). apply l39. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t80. exact h5. exact h6.
Qed.
Lemma t81 : p119 -> p118.
 intros h0.
 apply mul_firs with (1 := h0) ; finalize.
Qed.
Lemma l111 : p1 -> p2 -> p3 -> p4 -> p5 -> p118. (* BND(r * a2 - R * a2, [-1.55221e-10, 1.55221e-10]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p119). apply l112. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t81. exact h5.
Qed.
Lemma t82 : p115 -> p118 -> p114.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l107 : p1 -> p2 -> p3 -> p4 -> p5 -> p114. (* BND(float<24,-149,ne>(r * a2) - r * a2 + (r * a2 - R * a2), [-2.71637e-10, 2.71637e-10]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p115). apply l108. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p118). apply l111. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t82. exact h5. exact h6.
Qed.
Lemma t83 : p114 -> p113.
 intros h0.
 apply sub_xals with (1 := h0) ; finalize.
Qed.
Lemma l106 : p1 -> p2 -> p3 -> p4 -> p5 -> p113. (* BND(float<24,-149,ne>(r * a2) - R * a2, [-2.71637e-10, 2.71637e-10]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p114). apply l107. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t83. exact h5.
Qed.
Lemma t84 : p113 -> p112.
 intros h0.
 apply add_fils with (1 := h0) ; finalize.
Qed.
Lemma l105 : p1 -> p2 -> p3 -> p4 -> p5 -> p112. (* BND(a1 + float<24,-149,ne>(r * a2) - (a1 + R * a2), [-2.71637e-10, 2.71637e-10]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p113). apply l106. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t84. exact h5.
Qed.
Lemma t85 : p110 -> p112 -> p109.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l102 : p1 -> p2 -> p3 -> p4 -> p5 -> p109. (* BND(float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) - (a1 + float<24,-149,ne>(r * a2)) + (a1 + float<24,-149,ne>(r * a2) - (a1 + R * a2)), [-3.0074e-08, 3.0074e-08]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p110). apply l103. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p112). apply l105. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t85. exact h5. exact h6.
Qed.
Lemma t86 : p109 -> p108.
 intros h0.
 apply sub_xals with (1 := h0) ; finalize.
Qed.
Lemma l101 : p1 -> p2 -> p3 -> p4 -> p5 -> p108. (* BND(float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) - (a1 + R * a2), [-3.0074e-08, 3.0074e-08]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p109). apply l102. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t86. exact h5.
Qed.
Lemma t87 : p107 -> p108 -> p106.
 intros h0 h1.
 apply mul_po with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l99 : p1 -> p2 -> p3 -> p4 -> p5 -> p106. (* BND(R * R * (float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) - (a1 + R * a2)), [-1.41615e-11, 1.41615e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p107). apply l100. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p108). apply l101. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t87. exact h5. exact h6.
Qed.
Lemma t88 : p85 -> p106 -> p84.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l80 : p1 -> p2 -> p3 -> p4 -> p5 -> p84. (* BND((float<24,-149,ne>(r * r) - R * R) * float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) + R * R * (float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) - (a1 + R * a2)), [-4.18462e-11, 4.18462e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p85). apply l81. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p106). apply l99. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t88. exact h5. exact h6.
Qed.
Lemma t89 : p84 -> p83.
 intros h0.
 apply mul_mals with (1 := h0) ; finalize.
Qed.
Lemma l79 : p1 -> p2 -> p3 -> p4 -> p5 -> p83. (* BND(float<24,-149,ne>(r * r) * float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) - R * R * (a1 + R * a2), [-4.18462e-11, 4.18462e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p84). apply l80. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t89. exact h5.
Qed.
Lemma t90 : p79 -> p83 -> p78.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l74 : p1 -> p2 -> p3 -> p4 -> p5 -> p78. (* BND(q - float<24,-149,ne>(r * r) * float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) + (float<24,-149,ne>(r * r) * float<24,-149,ne>(a1 + float<24,-149,ne>(r * a2)) - R * R * (a1 + R * a2)), [-4.91222e-11, 4.91222e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p79). apply l75. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p83). apply l79. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t90. exact h5. exact h6.
Qed.
Lemma t91 : p78 -> p77.
 intros h0.
 apply sub_xals with (1 := h0) ; finalize.
Qed.
Lemma l73 : p1 -> p2 -> p3 -> p4 -> p5 -> p77. (* BND(q - R * R * (a1 + R * a2), [-4.91222e-11, 4.91222e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p78). apply l74. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t91. exact h5.
Qed.
Lemma t92 : p77 -> p76.
 intros h0.
 apply add_fils with (1 := h0) ; finalize.
Qed.
Lemma l72 : p1 -> p2 -> p3 -> p4 -> p5 -> p76. (* BND(r2 + q - (r2 + R * R * (a1 + R * a2)), [-4.91222e-11, 4.91222e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p77). apply l73. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t92. exact h5.
Qed.
Lemma t93 : p72 -> p76 -> p71.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l67 : p1 -> p2 -> p3 -> p4 -> p5 -> p71. (* BND(float<24,-149,ne>(r2 + q) - (r2 + q) + (r2 + q - (r2 + R * R * (a1 + R * a2))), [-7.8226e-11, 7.8226e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p72). apply l68. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p76). apply l72. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t93. exact h5. exact h6.
Qed.
Lemma t94 : p71 -> p70.
 intros h0.
 apply sub_xals with (1 := h0) ; finalize.
Qed.
Lemma l66 : p1 -> p2 -> p3 -> p4 -> p5 -> p70. (* BND(float<24,-149,ne>(r2 + q) - (r2 + R * R * (a1 + R * a2)), [-7.8226e-11, 7.8226e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p71). apply l67. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t94. exact h5.
Qed.
Lemma t95 : p70 -> p69.
 intros h0.
 apply add_fils with (1 := h0) ; finalize.
Qed.
Lemma l65 : p1 -> p2 -> p3 -> p4 -> p5 -> p69. (* BND(r1 + float<24,-149,ne>(r2 + q) - (r1 + (r2 + R * R * (a1 + R * a2))), [-7.8226e-11, 7.8226e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p70). apply l66. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t95. exact h5.
Qed.
Lemma t96 : p65 -> p69 -> p64.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l60 : p1 -> p2 -> p3 -> p4 -> p5 -> p64. (* BND(p - (r1 + float<24,-149,ne>(r2 + q)) + (r1 + float<24,-149,ne>(r2 + q) - (r1 + (r2 + R * R * (a1 + R * a2)))), [-1.00955e-09, 1.00955e-09]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p65). apply l61. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p69). apply l65. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t96. exact h5. exact h6.
Qed.
Lemma t97 : p64 -> p63.
 intros h0.
 apply sub_xals with (1 := h0) ; finalize.
Qed.
Lemma l59 : p1 -> p2 -> p3 -> p4 -> p5 -> p63. (* BND(p - (r1 + (r2 + R * R * (a1 + R * a2))), [-1.00955e-09, 1.00955e-09]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p64). apply l60. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t97. exact h5.
Qed.
Lemma t98 : p21 -> p63 -> p62.
 intros h0 h1.
 apply mul_po with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l58 : p1 -> p2 -> p3 -> p4 -> p5 -> p62. (* BND(S * (p - (r1 + (r2 + R * R * (a1 + R * a2)))), [-1.03165e-09, 1.03165e-09]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p21). apply l17. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p63). apply l59. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t98. exact h5. exact h6.
Qed.
Lemma t99 : p60 -> p62 -> p59.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l55 : p1 -> p2 -> p3 -> p4 -> p5 -> p59. (* BND((s - S) * p + S * (p - (r1 + (r2 + R * R * (a1 + R * a2)))), [-1.07537e-09, 2.13089e-09]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p60). apply l56. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p62). apply l58. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t99. exact h5. exact h6.
Qed.
Lemma t100 : p59 -> p58.
 intros h0.
 apply mul_mals with (1 := h0) ; finalize.
Qed.
Lemma l54 : p1 -> p2 -> p3 -> p4 -> p5 -> p58. (* BND(s * p - S * (r1 + (r2 + R * R * (a1 + R * a2))), [-1.07537e-09, 2.13089e-09]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p59). apply l55. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t100. exact h5.
Qed.
Lemma t101 : p54 -> p58 -> p53.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l49 : p1 -> p2 -> p3 -> p4 -> p5 -> p53. (* BND(float<24,-149,ne>(s * p) - s * p + (s * p - S * (r1 + (r2 + R * R * (a1 + R * a2)))), [-2.00669e-09, 3.06221e-09]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p54). apply l50. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p58). apply l54. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t101. exact h5. exact h6.
Qed.
Lemma t102 : p53 -> p52.
 intros h0.
 apply sub_xals with (1 := h0) ; finalize.
Qed.
Lemma l48 : p1 -> p2 -> p3 -> p4 -> p5 -> p52. (* BND(float<24,-149,ne>(s * p) - S * (r1 + (r2 + R * R * (a1 + R * a2))), [-2.00669e-09, 3.06221e-09]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p53). apply l49. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t102. exact h5.
Qed.
Lemma t103 : p52 -> p51.
 intros h0.
 apply add_fils with (1 := h0) ; finalize.
Qed.
Lemma l47 : p1 -> p2 -> p3 -> p4 -> p5 -> p51. (* BND(s2 + float<24,-149,ne>(s * p) - (s2 + S * (r1 + (r2 + R * R * (a1 + R * a2)))), [-2.00669e-09, 3.06221e-09]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p52). apply l48. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t103. exact h5.
Qed.
Lemma t104 : p47 -> p51 -> p46.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l42 : p1 -> p2 -> p3 -> p4 -> p5 -> p46. (* BND(float<24,-149,ne>(s2 + float<24,-149,ne>(s * p)) - (s2 + float<24,-149,ne>(s * p)) + (s2 + float<24,-149,ne>(s * p) - (s2 + S * (r1 + (r2 + R * R * (a1 + R * a2))))), [-2.93802e-09, 3.99354e-09]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p47). apply l43. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p51). apply l47. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t104. exact h5. exact h6.
Qed.
Lemma t105 : p46 -> p45.
 intros h0.
 apply sub_xals with (1 := h0) ; finalize.
Qed.
Lemma l41 : p1 -> p2 -> p3 -> p4 -> p5 -> p45. (* BND(float<24,-149,ne>(s2 + float<24,-149,ne>(s * p)) - (s2 + S * (r1 + (r2 + R * R * (a1 + R * a2)))), [-2.93802e-09, 3.99354e-09]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p46). apply l42. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t105. exact h5.
Qed.
Lemma t106 : p45 -> p44.
 intros h0.
 apply add_fils with (1 := h0) ; finalize.
Qed.
Lemma l40 : p1 -> p2 -> p3 -> p4 -> p5 -> p44. (* BND(s1 + float<24,-149,ne>(s2 + float<24,-149,ne>(s * p)) - E, [-2.93802e-09, 3.99354e-09]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p45). apply l41. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t106. exact h5.
Qed.
Lemma t107 : p11 -> p44 -> p10.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l6 : p1 -> p2 -> p3 -> p4 -> p5 -> p10. (* BND(e - (s1 + float<24,-149,ne>(s2 + float<24,-149,ne>(s * p))) + (s1 + float<24,-149,ne>(s2 + float<24,-149,ne>(s * p)) - E), [-6.25427e-08, 6.35982e-08]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p11). apply l7. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p44). apply l40. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t107. exact h5. exact h6.
Qed.
Lemma t108 : p10 -> p9.
 intros h0.
 apply sub_xals with (1 := h0) ; finalize.
Qed.
Lemma l5 : p1 -> p2 -> p3 -> p4 -> p5 -> p9. (* BND(e - E, [-6.25427e-08, 6.35982e-08]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p10). apply l6. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t108. exact h5.
Qed.
Definition f106 := Float2 (809547673066441717) (-92).
Definition f107 := Float2 (-809547673066441717) (-92).
Definition i79 := makepairF f107 f106.
Notation p120 := (BND r2 i79). (* BND(Er - E0, [-1.63487e-10, 1.63487e-10]) *)
Notation r126 := ((r72 * r4)%R).
Notation r128 := ((r20 - r4)%R).
Notation r127 := ((_S * r128)%R).
Notation r125 := ((r127 + r126)%R).
Notation p121 := (BND r125 i79). (* BND(S * (1 + R + a1 * R * R + a2 * R * R * R + 0 - (1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0 + Z)) + (S - S0) * (1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0 + Z), [-1.63487e-10, 1.63487e-10]) *)
Definition f108 := Float2 (50452904698886279) (-88).
Definition f109 := Float2 (-50452904698886279) (-88).
Definition i80 := makepairF f109 f108.
Notation p122 := (BND r127 i80). (* BND(S * (1 + R + a1 * R * R + a2 * R * R * R + 0 - (1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0 + Z)), [-1.63022e-10, 1.63022e-10]) *)
Definition f110 := Float2 (789948847832073737) (-92).
Definition f111 := Float2 (-789948847832073737) (-92).
Definition i81 := makepairF f111 f110.
Notation p123 := (BND r128 i81). (* BND(1 + R + a1 * R * R + a2 * R * R * R + 0 - (1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0 + Z), [-1.59529e-10, 1.59529e-10]) *)
Notation r130 := ((r21 - _Z)%R).
Notation r131 := ((r22 - r6)%R).
Notation r129 := ((r131 + r130)%R).
Notation p124 := (BND r129 i81). (* BND(1 + R + a1 * R * R + a2 * R * R * R - (1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0) + (0 - Z), [-1.59529e-10, 1.59529e-10]) *)
Definition f112 := Float2 (589105777642638353) (-93).
Definition f113 := Float2 (-589105777642638353) (-93).
Definition i82 := makepairF f113 f112.
Notation p125 := (BND r131 i82). (* BND(1 + R + a1 * R * R + a2 * R * R * R - (1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0), [-5.94845e-11, 5.94845e-11]) *)
Notation r133 := ((r23 - r7)%R).
Notation r134 := ((r33 - r12)%R).
Notation r132 := ((r134 + r133)%R).
Notation p126 := (BND r132 i82). (* BND(1 + R + a1 * R * R - (1 + R0 + a1 * R0 * R0) + (a2 * R * R * R - a2 * R0 * R0 * R0), [-5.94845e-11, 5.94845e-11]) *)
Definition f114 := Float2 (294485026023822501) (-92).
Definition f115 := Float2 (-294485026023822501) (-92).
Definition i83 := makepairF f115 f114.
Notation p127 := (BND r134 i83). (* BND(1 + R + a1 * R * R - (1 + R0 + a1 * R0 * R0), [-5.94708e-11, 5.94708e-11]) *)
Notation r136 := ((r34 - r13)%R).
Notation r137 := ((r36 - r16)%R).
Notation r135 := ((r137 + r136)%R).
Notation p128 := (BND r135 i83). (* BND(1 + R - (1 + R0) + (a1 * R * R - a1 * R0 * R0), [-5.94708e-11, 5.94708e-11]) *)
Notation p129 := (BND r137 i5). (* BND(1 + R - (1 + R0), [-5.82077e-11, 5.82077e-11]) *)
Lemma t109 : p5 -> p129.
 intros h0.
 apply add_fils with (1 := h0) ; finalize.
Qed.
Lemma l122 : p1 -> p2 -> p3 -> p4 -> p5 -> p129. (* BND(1 + R - (1 + R0), [-5.82077e-11, 5.82077e-11]) *)
 intros h0 h1 h2 h3 h4.
 apply t109. exact h4.
Qed.
Definition f116 := Float2 (800595183630176855) (-99).
Definition f117 := Float2 (-800595183630176855) (-99).
Definition i84 := makepairF f117 f116.
Notation p130 := (BND r136 i84). (* BND(a1 * R * R - a1 * R0 * R0, [-1.26312e-12, 1.26312e-12]) *)
Notation r140 := ((r35 - r14)%R).
Notation r139 := ((r140 * _R0)%R).
Notation r141 := ((r35 * r73)%R).
Notation r138 := ((r141 + r139)%R).
Notation p131 := (BND r138 i84). (* BND(a1 * R * (R - R0) + (a1 * R - a1 * R0) * R0, [-1.26312e-12, 1.26312e-12]) *)
Definition f118 := Float2 (800595182556426327) (-100).
Definition f119 := Float2 (-800595182556426327) (-100).
Definition i85 := makepairF f119 f118.
Notation p132 := (BND r141 i85). (* BND(a1 * R * (R - R0), [-6.31558e-13, 6.31558e-13]) *)
Definition f120 := Float2 (800595182556426327) (-66).
Definition i86 := makepairF f4 f120.
Notation p133 := (BND r35 i86). (* BND(a1 * R, [0, 0.0108501]) *)
Lemma t110 : p40 -> p2 -> p133.
 intros h0 h1.
 apply mul_pp with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l126 : p1 -> p2 -> p3 -> p4 -> p5 -> p133. (* BND(a1 * R, [0, 0.0108501]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p40). apply l36. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t110. exact h5. exact h1.
Qed.
Lemma t111 : p133 -> p5 -> p132.
 intros h0 h1.
 apply mul_po with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l125 : p1 -> p2 -> p3 -> p4 -> p5 -> p132. (* BND(a1 * R * (R - R0), [-6.31558e-13, 6.31558e-13]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p133). apply l126. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t111. exact h5. exact h4.
Qed.
Definition f121 := Float2 (800595184703927383) (-100).
Definition f122 := Float2 (-800595184703927383) (-100).
Definition i87 := makepairF f122 f121.
Notation p134 := (BND r139 i87). (* BND((a1 * R - a1 * R0) * R0, [-6.31558e-13, 6.31558e-13]) *)
Definition f123 := Float2 (2097169) (-56).
Definition f124 := Float2 (-2097169) (-56).
Definition i88 := makepairF f124 f123.
Notation p135 := (BND r140 i88). (* BND(a1 * R - a1 * R0, [-2.91041e-11, 2.91041e-11]) *)
Notation r142 := ((_a1 * r73)%R).
Notation p136 := (BND r142 i88). (* BND(a1 * (R - R0), [-2.91041e-11, 2.91041e-11]) *)
Lemma t112 : p40 -> p5 -> p136.
 intros h0 h1.
 apply mul_po with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l129 : p1 -> p2 -> p3 -> p4 -> p5 -> p136. (* BND(a1 * (R - R0), [-2.91041e-11, 2.91041e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p40). apply l36. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t112. exact h5. exact h4.
Qed.
Lemma t113 : p136 -> p135.
 intros h0.
 apply mul_fils with (1 := h0) ; finalize.
Qed.
Lemma l128 : p1 -> p2 -> p3 -> p4 -> p5 -> p135. (* BND(a1 * R - a1 * R0, [-2.91041e-11, 2.91041e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p136). apply l129. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t113. exact h5.
Qed.
Definition f125 := Float2 (800588694946478189) (-65).
Definition i89 := makepairF f10 f125.
Notation p137 := (BND _R0 i89). (* BND(R0, [-5.82077e-11, 0.0217]) *)
Notation r143 := ((_R - r73)%R).
Notation p138 := (BND r143 i89). (* BND(R - (R - R0), [-5.82077e-11, 0.0217]) *)
Lemma t114 : p2 -> p5 -> p138.
 intros h0 h1.
 apply sub with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l131 : p1 -> p2 -> p3 -> p4 -> p5 -> p138. (* BND(R - (R - R0), [-5.82077e-11, 0.0217]) *)
 intros h0 h1 h2 h3 h4.
 apply t114. exact h1. exact h4.
Qed.
Lemma t115 : p138 -> p137.
 intros h0.
 apply val_xebs with (1 := h0) ; finalize.
Qed.
Lemma l130 : p1 -> p2 -> p3 -> p4 -> p5 -> p137. (* BND(R0, [-5.82077e-11, 0.0217]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p138). apply l131. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t115. exact h5.
Qed.
Lemma t116 : p135 -> p137 -> p134.
 intros h0 h1.
 apply mul_oo with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l127 : p1 -> p2 -> p3 -> p4 -> p5 -> p134. (* BND((a1 * R - a1 * R0) * R0, [-6.31558e-13, 6.31558e-13]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p135). apply l128. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p137). apply l130. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t116. exact h5. exact h6.
Qed.
Lemma t117 : p132 -> p134 -> p131.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l124 : p1 -> p2 -> p3 -> p4 -> p5 -> p131. (* BND(a1 * R * (R - R0) + (a1 * R - a1 * R0) * R0, [-1.26312e-12, 1.26312e-12]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p132). apply l125. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p134). apply l127. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t117. exact h5. exact h6.
Qed.
Lemma t118 : p131 -> p130.
 intros h0.
 apply mul_mars with (1 := h0) ; finalize.
Qed.
Lemma l123 : p1 -> p2 -> p3 -> p4 -> p5 -> p130. (* BND(a1 * R * R - a1 * R0 * R0, [-1.26312e-12, 1.26312e-12]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p131). apply l124. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t118. exact h5.
Qed.
Lemma t119 : p129 -> p130 -> p128.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l121 : p1 -> p2 -> p3 -> p4 -> p5 -> p128. (* BND(1 + R - (1 + R0) + (a1 * R * R - a1 * R0 * R0), [-5.94708e-11, 5.94708e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p129). apply l122. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p130). apply l123. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t119. exact h5. exact h6.
Qed.
Lemma t120 : p128 -> p127.
 intros h0.
 apply add_mibs with (1 := h0) ; finalize.
Qed.
Lemma l120 : p1 -> p2 -> p3 -> p4 -> p5 -> p127. (* BND(1 + R + a1 * R * R - (1 + R0 + a1 * R0 * R0), [-5.94708e-11, 5.94708e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p128). apply l121. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t120. exact h5.
Qed.
Definition f126 := Float2 (277966018546380929) (-104).
Definition f127 := Float2 (-277966018546380929) (-104).
Definition i90 := makepairF f127 f126.
Notation p139 := (BND r133 i90). (* BND(a2 * R * R * R - a2 * R0 * R0 * R0, [-1.37048e-14, 1.37048e-14]) *)
Notation r145 := ((r9 * r73)%R).
Notation r147 := ((r31 - r9)%R).
Notation r146 := ((r147 * _R)%R).
Notation r144 := ((r146 + r145)%R).
Notation p140 := (BND r144 i90). (* BND((a2 * R * R - a2 * R0 * R0) * R + a2 * R0 * R0 * (R - R0), [-1.37048e-14, 1.37048e-14]) *)
Definition f128 := Float2 (741242715129534905) (-106).
Definition f129 := Float2 (-741242715129534905) (-106).
Definition i91 := makepairF f129 f128.
Notation p141 := (BND r146 i91). (* BND((a2 * R * R - a2 * R0 * R0) * R, [-9.13652e-15, 9.13652e-15]) *)
Definition f130 := Float2 (1067457827087463859) (-101).
Definition f131 := Float2 (-1067457827087463859) (-101).
Definition i92 := makepairF f131 f130.
Notation p142 := (BND r147 i92). (* BND(a2 * R * R - a2 * R0 * R0, [-4.21038e-13, 4.21038e-13]) *)
Notation r150 := ((r32 - r10)%R).
Notation r149 := ((r150 * _R0)%R).
Notation r151 := ((r32 * r73)%R).
Notation r148 := ((r151 + r149)%R).
Notation p143 := (BND r148 i92). (* BND(a2 * R * (R - R0) + (a2 * R - a2 * R0) * R0, [-4.21038e-13, 4.21038e-13]) *)
Definition f132 := Float2 (1067457825655799731) (-102).
Definition f133 := Float2 (-1067457825655799731) (-102).
Definition i93 := makepairF f133 f132.
Notation p144 := (BND r151 i93). (* BND(a2 * R * (R - R0), [-2.10519e-13, 2.10519e-13]) *)
Definition f134 := Float2 (1067457825655799731) (-68).
Definition i94 := makepairF f4 f134.
Notation p145 := (BND r32 i94). (* BND(a2 * R, [0, 0.00361669]) *)
Lemma t121 : p43 -> p2 -> p145.
 intros h0 h1.
 apply mul_pp with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l138 : p1 -> p2 -> p3 -> p4 -> p5 -> p145. (* BND(a2 * R, [0, 0.00361669]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p43). apply l39. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t121. exact h5. exact h1.
Qed.
Lemma t122 : p145 -> p5 -> p144.
 intros h0 h1.
 apply mul_po with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l137 : p1 -> p2 -> p3 -> p4 -> p5 -> p144. (* BND(a2 * R * (R - R0), [-2.10519e-13, 2.10519e-13]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p145). apply l138. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t122. exact h5. exact h4.
Qed.
Definition f135 := Float2 (1067457828519127987) (-102).
Definition f136 := Float2 (-1067457828519127987) (-102).
Definition i95 := makepairF f136 f135.
Notation p146 := (BND r149 i95). (* BND((a2 * R - a2 * R0) * R0, [-2.10519e-13, 2.10519e-13]) *)
Definition f137 := Float2 (2796219) (-58).
Definition f138 := Float2 (-2796219) (-58).
Definition i96 := makepairF f138 f137.
Notation p147 := (BND r150 i96). (* BND(a2 * R - a2 * R0, [-9.70133e-12, 9.70133e-12]) *)
Notation r152 := ((_a2 * r73)%R).
Notation p148 := (BND r152 i96). (* BND(a2 * (R - R0), [-9.70133e-12, 9.70133e-12]) *)
Lemma t123 : p43 -> p5 -> p148.
 intros h0 h1.
 apply mul_po with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l141 : p1 -> p2 -> p3 -> p4 -> p5 -> p148. (* BND(a2 * (R - R0), [-9.70133e-12, 9.70133e-12]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p43). apply l39. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t123. exact h5. exact h4.
Qed.
Lemma t124 : p148 -> p147.
 intros h0.
 apply mul_fils with (1 := h0) ; finalize.
Qed.
Lemma l140 : p1 -> p2 -> p3 -> p4 -> p5 -> p147. (* BND(a2 * R - a2 * R0, [-9.70133e-12, 9.70133e-12]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p148). apply l141. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t124. exact h5.
Qed.
Lemma t125 : p147 -> p137 -> p146.
 intros h0 h1.
 apply mul_oo with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l139 : p1 -> p2 -> p3 -> p4 -> p5 -> p146. (* BND((a2 * R - a2 * R0) * R0, [-2.10519e-13, 2.10519e-13]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p147). apply l140. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p137). apply l130. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t125. exact h5. exact h6.
Qed.
Lemma t126 : p144 -> p146 -> p143.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l136 : p1 -> p2 -> p3 -> p4 -> p5 -> p143. (* BND(a2 * R * (R - R0) + (a2 * R - a2 * R0) * R0, [-4.21038e-13, 4.21038e-13]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p144). apply l137. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p146). apply l139. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t126. exact h5. exact h6.
Qed.
Lemma t127 : p143 -> p142.
 intros h0.
 apply mul_mars with (1 := h0) ; finalize.
Qed.
Lemma l135 : p1 -> p2 -> p3 -> p4 -> p5 -> p142. (* BND(a2 * R * R - a2 * R0 * R0, [-4.21038e-13, 4.21038e-13]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p143). apply l136. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t127. exact h5.
Qed.
Lemma t128 : p142 -> p2 -> p141.
 intros h0 h1.
 apply mul_op with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l134 : p1 -> p2 -> p3 -> p4 -> p5 -> p141. (* BND((a2 * R * R - a2 * R0 * R0) * R, [-9.13652e-15, 9.13652e-15]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p142). apply l135. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t128. exact h5. exact h1.
Qed.
Definition f139 := Float2 (370621359055988811) (-106).
Definition f140 := Float2 (-370621359055988811) (-106).
Definition i97 := makepairF f140 f139.
Notation p149 := (BND r145 i97). (* BND(a2 * R0 * R0 * (R - R0), [-4.56826e-15, 4.56826e-15]) *)
Definition f141 := Float2 (370621359055988811) (-72).
Definition i98 := makepairF f136 f141.
Notation p150 := (BND r9 i98). (* BND(a2 * R0 * R0, [-2.10519e-13, 7.84821e-05]) *)
Definition f142 := Float2 (1067457828519127987) (-68).
Definition i99 := makepairF f138 f142.
Notation p151 := (BND r10 i99). (* BND(a2 * R0, [-9.70133e-12, 0.00361669]) *)
Lemma t129 : p43 -> p137 -> p151.
 intros h0 h1.
 apply mul_po with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l144 : p1 -> p2 -> p3 -> p4 -> p5 -> p151. (* BND(a2 * R0, [-9.70133e-12, 0.00361669]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p43). apply l39. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p137). apply l130. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t129. exact h5. exact h6.
Qed.
Lemma t130 : p151 -> p137 -> p150.
 intros h0 h1.
 apply mul_oo with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l143 : p1 -> p2 -> p3 -> p4 -> p5 -> p150. (* BND(a2 * R0 * R0, [-2.10519e-13, 7.84821e-05]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p151). apply l144. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p137). apply l130. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t130. exact h5. exact h6.
Qed.
Lemma t131 : p150 -> p5 -> p149.
 intros h0 h1.
 apply mul_oo with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l142 : p1 -> p2 -> p3 -> p4 -> p5 -> p149. (* BND(a2 * R0 * R0 * (R - R0), [-4.56826e-15, 4.56826e-15]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p150). apply l143. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t131. exact h5. exact h4.
Qed.
Lemma t132 : p141 -> p149 -> p140.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l133 : p1 -> p2 -> p3 -> p4 -> p5 -> p140. (* BND((a2 * R * R - a2 * R0 * R0) * R + a2 * R0 * R0 * (R - R0), [-1.37048e-14, 1.37048e-14]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p141). apply l134. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p149). apply l142. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t132. exact h5. exact h6.
Qed.
Lemma t133 : p140 -> p139.
 intros h0.
 apply mul_mals with (1 := h0) ; finalize.
Qed.
Lemma l132 : p1 -> p2 -> p3 -> p4 -> p5 -> p139. (* BND(a2 * R * R * R - a2 * R0 * R0 * R0, [-1.37048e-14, 1.37048e-14]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p140). apply l133. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t133. exact h5.
Qed.
Lemma t134 : p127 -> p139 -> p126.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l119 : p1 -> p2 -> p3 -> p4 -> p5 -> p126. (* BND(1 + R + a1 * R * R - (1 + R0 + a1 * R0 * R0) + (a2 * R * R * R - a2 * R0 * R0 * R0), [-5.94845e-11, 5.94845e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p127). apply l120. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p139). apply l132. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t134. exact h5. exact h6.
Qed.
Lemma t135 : p126 -> p125.
 intros h0.
 apply add_mibs with (1 := h0) ; finalize.
Qed.
Lemma l118 : p1 -> p2 -> p3 -> p4 -> p5 -> p125. (* BND(1 + R + a1 * R * R + a2 * R * R * R - (1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0), [-5.94845e-11, 5.94845e-11]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p126). apply l119. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t135. exact h5.
Qed.
Notation p152 := (BND r130 i3). (* BND(0 - Z, [-1.00044e-10, 1.00044e-10]) *)
Definition i100 := makepairF f4 f4.
Notation p153 := (BND r21 i100). (* BND(0, [0, 0]) *)
Lemma t136 : p153.
 apply constant1 ; finalize.
Qed.
Lemma l146 : p1 -> p2 -> p3 -> p4 -> p5 -> p153. (* BND(0, [0, 0]) *)
 intros h0 h1 h2 h3 h4.
 apply t136.
Qed.
Lemma t137 : p153 -> p3 -> p152.
 intros h0 h1.
 apply sub with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l145 : p1 -> p2 -> p3 -> p4 -> p5 -> p152. (* BND(0 - Z, [-1.00044e-10, 1.00044e-10]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p153). apply l146. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t137. exact h5. exact h2.
Qed.
Lemma t138 : p125 -> p152 -> p124.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l117 : p1 -> p2 -> p3 -> p4 -> p5 -> p124. (* BND(1 + R + a1 * R * R + a2 * R * R * R - (1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0) + (0 - Z), [-1.59529e-10, 1.59529e-10]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p125). apply l118. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p152). apply l145. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t138. exact h5. exact h6.
Qed.
Lemma t139 : p124 -> p123.
 intros h0.
 apply add_mibs with (1 := h0) ; finalize.
Qed.
Lemma l116 : p1 -> p2 -> p3 -> p4 -> p5 -> p123. (* BND(1 + R + a1 * R * R + a2 * R * R * R + 0 - (1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0 + Z), [-1.59529e-10, 1.59529e-10]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p124). apply l117. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t139. exact h5.
Qed.
Lemma t140 : p21 -> p123 -> p122.
 intros h0 h1.
 apply mul_po with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l115 : p1 -> p2 -> p3 -> p4 -> p5 -> p122. (* BND(S * (1 + R + a1 * R * R + a2 * R * R * R + 0 - (1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0 + Z)), [-1.63022e-10, 1.63022e-10]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p21). apply l17. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p123). apply l116. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t140. exact h5. exact h6.
Qed.
Definition f143 := Float2 (147276664592720129) (-98).
Definition f144 := Float2 (-147276664592720129) (-98).
Definition i101 := makepairF f144 f143.
Notation p154 := (BND r126 i101). (* BND((S - S0) * (1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0 + Z), [-4.64723e-13, 4.64723e-13]) *)
Definition f145 := Float2 (147276664592720129) (-57).
Definition f146 := Float2 (1152921504423661347) (-60).
Definition i102 := makepairF f146 f145.
Notation p155 := (BND r4 i102). (* BND(1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0 + Z, [1, 1.02194]) *)
Definition f147 := Float2 (147276664578302209) (-57).
Definition f148 := Float2 (1152921504539004707) (-60).
Definition i103 := makepairF f148 f147.
Notation p156 := (BND r6 i103). (* BND(1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0, [1, 1.02194]) *)
Definition f149 := Float2 (589105676564733117) (-59).
Definition f150 := Float2 (576460752269504987) (-59).
Definition i104 := makepairF f150 f149.
Notation p157 := (BND r12 i104). (* BND(1 + R0 + a1 * R0 * R0, [1, 1.02194]) *)
Definition f151 := Float2 (294484975330981105) (-58).
Definition f152 := Float2 (17179869183) (-34).
Definition i105 := makepairF f152 f151.
Notation p158 := (BND r16 i105). (* BND(1 + R0, [1, 1.0217]) *)
Definition f153 := Float2 (1) (0).
Definition i106 := makepairF f153 f153.
Notation p159 := (BND r17 i106). (* BND(1, [1, 1]) *)
Lemma t141 : p159.
 apply constant1 ; finalize.
Qed.
Lemma l152 : p1 -> p2 -> p3 -> p4 -> p5 -> p159. (* BND(1, [1, 1]) *)
 intros h0 h1 h2 h3 h4.
 apply t141.
Qed.
Lemma t142 : p159 -> p137 -> p158.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l151 : p1 -> p2 -> p3 -> p4 -> p5 -> p158. (* BND(1 + R0, [1, 1.0217]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p159). apply l152. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p137). apply l130. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t142. exact h5. exact h6.
Qed.
Definition f154 := Float2 (555933297749631913) (-71).
Definition i107 := makepairF f122 f154.
Notation p160 := (BND r13 i107). (* BND(a1 * R0 * R0, [-6.31558e-13, 0.000235447]) *)
Definition f155 := Float2 (800595184703927383) (-66).
Definition i108 := makepairF f124 f155.
Notation p161 := (BND r14 i108). (* BND(a1 * R0, [-2.91041e-11, 0.0108501]) *)
Lemma t143 : p40 -> p137 -> p161.
 intros h0 h1.
 apply mul_po with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l154 : p1 -> p2 -> p3 -> p4 -> p5 -> p161. (* BND(a1 * R0, [-2.91041e-11, 0.0108501]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p40). apply l36. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p137). apply l130. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t143. exact h5. exact h6.
Qed.
Lemma t144 : p161 -> p137 -> p160.
 intros h0 h1.
 apply mul_oo with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l153 : p1 -> p2 -> p3 -> p4 -> p5 -> p160. (* BND(a1 * R0 * R0, [-6.31558e-13, 0.000235447]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p161). apply l154. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p137). apply l130. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t144. exact h5. exact h6.
Qed.
Lemma t145 : p158 -> p160 -> p157.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l150 : p1 -> p2 -> p3 -> p4 -> p5 -> p157. (* BND(1 + R0 + a1 * R0 * R0, [1, 1.02194]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p158). apply l151. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p160). apply l153. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t145. exact h5. exact h6.
Qed.
Definition f156 := Float2 (514718944837629415) (-78).
Definition i109 := makepairF f140 f156.
Notation p162 := (BND r7 i109). (* BND(a2 * R0 * R0 * R0, [-4.56826e-15, 1.70306e-06]) *)
Lemma t146 : p150 -> p137 -> p162.
 intros h0 h1.
 apply mul_oo with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l155 : p1 -> p2 -> p3 -> p4 -> p5 -> p162. (* BND(a2 * R0 * R0 * R0, [-4.56826e-15, 1.70306e-06]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p150). apply l143. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p137). apply l130. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t146. exact h5. exact h6.
Qed.
Lemma t147 : p157 -> p162 -> p156.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l149 : p1 -> p2 -> p3 -> p4 -> p5 -> p156. (* BND(1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0, [1, 1.02194]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p157). apply l150. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p162). apply l155. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t147. exact h5. exact h6.
Qed.
Lemma t148 : p156 -> p3 -> p155.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l148 : p1 -> p2 -> p3 -> p4 -> p5 -> p155. (* BND(1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0 + Z, [1, 1.02194]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p156). apply l149. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t148. exact h5. exact h2.
Qed.
Lemma t149 : p4 -> p155 -> p154.
 intros h0 h1.
 apply mul_op with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l147 : p1 -> p2 -> p3 -> p4 -> p5 -> p154. (* BND((S - S0) * (1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0 + Z), [-4.64723e-13, 4.64723e-13]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p155). apply l148. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t149. exact h3. exact h5.
Qed.
Lemma t150 : p122 -> p154 -> p121.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l114 : p1 -> p2 -> p3 -> p4 -> p5 -> p121. (* BND(S * (1 + R + a1 * R * R + a2 * R * R * R + 0 - (1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0 + Z)) + (S - S0) * (1 + R0 + a1 * R0 * R0 + a2 * R0 * R0 * R0 + Z), [-1.63487e-10, 1.63487e-10]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p122). apply l115. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p154). apply l147. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t150. exact h5. exact h6.
Qed.
Lemma t151 : p121 -> p120.
 intros h0.
 apply mul_mars with (1 := h0) ; finalize.
Qed.
Lemma l113 : p1 -> p2 -> p3 -> p4 -> p5 -> p120. (* BND(Er - E0, [-1.63487e-10, 1.63487e-10]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p121). apply l114. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t151. exact h5.
Qed.
Lemma t152 : p9 -> p120 -> p8.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l4 : p1 -> p2 -> p3 -> p4 -> p5 -> p8. (* BND(e - E + (Er - E0), [-6.27061e-08, 6.37617e-08]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p9). apply l5. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p120). apply l113. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t152. exact h5. exact h6.
Qed.
Lemma t153 : p7 -> p8 -> p6.
 intros h0 h1.
 apply bnd_rewrite with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l2 : p1 -> p2 -> p3 -> p4 -> p5 -> p6. (* BND(e - E0, [-6.27061e-08, 6.37617e-08]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p7). apply l3. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p8). apply l4. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t153. exact h5. exact h6.
Qed.
Lemma l1 : p1 -> p2 -> p3 -> p4 -> p5 -> p6. (* BND(e - E0, [-6.27061e-08, 6.37617e-08]) *)
 intros h0 h1 h2 h3 h4.
 apply l2. exact h0. exact h1. exact h2. exact h3. exact h4.
Qed.
Definition f157 := Float2 (4380173) (-22).
Definition i110 := makepairF f29 f157.
Notation p163 := (BND _e i110). (* BND(e, [1.0219, 1.04431]) *)
Definition f158 := Float2 (602006451198709857) (-59).
Definition f159 := Float2 (589083562848366111) (-59).
Definition i111 := makepairF f159 f158.
Notation p164 := (BND _e i111). (* BND(e, [1.0219, 1.04431]) *)
Notation r153 := ((_E0 + r70)%R).
Notation p165 := (BND r153 i111). (* BND(E0 + (e - E0), [1.0219, 1.04431]) *)
Definition f160 := Float2 (602006414442610307) (-59).
Definition f161 := Float2 (147270899749000007) (-57).
Definition i112 := makepairF f161 f160.
Notation p166 := (BND _E0 i112). (* BND(E0, [1.0219, 1.04431]) *)
Definition f162 := Float2 (17977404757389) (-44).
Definition f163 := Float2 (17977404757373) (-44).
Definition i113 := makepairF f163 f162.
Notation p167 := (BND _S0 i113). (* BND(S0, [1.0219, 1.0219]) *)
Notation r154 := ((_S - r72)%R).
Notation p168 := (BND r154 i113). (* BND(S - (S - S0), [1.0219, 1.0219]) *)
Lemma t154 : p21 -> p4 -> p168.
 intros h0 h1.
 apply sub with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l162 : p1 -> p2 -> p3 -> p4 -> p5 -> p168. (* BND(S - (S - S0), [1.0219, 1.0219]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p21). apply l17. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t154. exact h5. exact h3.
Qed.
Lemma t155 : p168 -> p167.
 intros h0.
 apply val_xebs with (1 := h0) ; finalize.
Qed.
Lemma l161 : p1 -> p2 -> p3 -> p4 -> p5 -> p167. (* BND(S0, [1.0219, 1.0219]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p168). apply l162. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t155. exact h5.
Qed.
Lemma t156 : p167 -> p155 -> p166.
 intros h0 h1.
 apply mul_pp with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l160 : p1 -> p2 -> p3 -> p4 -> p5 -> p166. (* BND(E0, [1.0219, 1.04431]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p167). apply l161. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p155). apply l148. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t156. exact h5. exact h6.
Qed.
Lemma t157 : p166 -> p6 -> p165.
 intros h0 h1.
 apply add with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l159 : p1 -> p2 -> p3 -> p4 -> p5 -> p165. (* BND(E0 + (e - E0), [1.0219, 1.04431]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p166). apply l160. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p6). apply l2. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t157. exact h5. exact h6.
Qed.
Lemma t158 : p165 -> p164.
 intros h0.
 apply val_xabs with (1 := h0) ; finalize.
Qed.
Lemma l158 : p1 -> p2 -> p3 -> p4 -> p5 -> p164. (* BND(e, [1.0219, 1.04431]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p165). apply l159. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t158. exact h5.
Qed.
Notation p169 := (FIX _e (-23)). (* FIX(e, -23) *)
Notation p170 := (FLT _e (24)). (* FLT(e, 24) *)
Lemma t159 : p170.
 apply flt_of_float ; finalize.
Qed.
Lemma l164 : p1 -> p2 -> p3 -> p4 -> p5 -> p170. (* FLT(e, 24) *)
 intros h0 h1 h2 h3 h4.
 apply t159.
Qed.
Definition f164 := Float2 (8768135) (-23).
Definition f165 := Float2 (4282253) (-22).
Definition i114 := makepairF f165 f164.
Notation p171 := (ABS _e i114). (* ABS(e, [1.02097, 1.04524]) *)
Notation p172 := (BND _e i114). (* BND(e, [1.02097, 1.04524]) *)
Lemma t160 : p13 -> p172.
 intros h0.
 apply float_round_ne with (1 := h0) ; finalize.
Qed.
Lemma l166 : p1 -> p2 -> p3 -> p4 -> p5 -> p172. (* BND(e, [1.02097, 1.04524]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p13). apply l9. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t160. exact h5.
Qed.
Lemma t161 : p172 -> p171.
 intros h0.
 apply abs_of_bnd_p with (1 := h0) ; finalize.
Qed.
Lemma l165 : p1 -> p2 -> p3 -> p4 -> p5 -> p171. (* ABS(e, [1.02097, 1.04524]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p172). apply l166. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t161. exact h5.
Qed.
Lemma t162 : p170 -> p171 -> p169.
 intros h0 h1.
 apply fix_of_flt_bnd with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l163 : p1 -> p2 -> p3 -> p4 -> p5 -> p169. (* FIX(e, -23) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p170). apply l164. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p171). apply l165. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t162. exact h5. exact h6.
Qed.
Lemma t163 : p164 -> p169 -> p163.
 intros h0 h1.
 apply bnd_of_bnd_fix with (1 := h0) (2 := h1) ; finalize.
Qed.
Lemma l157 : p1 -> p2 -> p3 -> p4 -> p5 -> p163. (* BND(e, [1.0219, 1.04431]) *)
 intros h0 h1 h2 h3 h4.
 assert (h5 : p164). apply l158. exact h0. exact h1. exact h2. exact h3. exact h4.
 assert (h6 : p169). apply l163. exact h0. exact h1. exact h2. exact h3. exact h4.
 apply t163. exact h5. exact h6.
Qed.
Lemma l156 : p1 -> p2 -> p3 -> p4 -> p5 -> p163. (* BND(e, [1.0219, 1.04431]) *)
 intros h0 h1 h2 h3 h4.
 apply l157. exact h0. exact h1. exact h2. exact h3. exact h4.
Qed.
End Generated_by_Gappa.
