Definition PathFrom {t} (x : t) := exists y, x = y.
Locate "exists".

Theorem J: forall {t} (x : t), forall P : PathFrom x -> Prop, 
    (exists path1, P path1) -> forall path2, P path2.
intros t x P H path2.
destruct H.
destruct x0; destruct path2.
rewrite <- e0.
rewrite <- e in H.
assumption.
Qed. 
