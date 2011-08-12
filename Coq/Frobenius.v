Module Frobenius.
Section Proof.

Variables A B : Prop.
Variable P : B -> Prop.

Lemma Frobenius : (A /\ (exists b : B, P b)) <-> exists b : B, A /\ P b.
  split.
    intro H; elim H.
    intros a b_exists; elim b_exists.
    intros b P_b; exists b.
    auto.
  
    intro H; elim H.
    intros b P_b. 
    split.
      tauto.
  
      exists b.
      tauto.
Qed.

End Proof.
End Frobenius.