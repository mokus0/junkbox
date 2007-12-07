class Invertible f a b where
	apply :: (Invertible f a b) => f -> a -> b
	invert :: forall a b. forall g. (Invertible f a b, Invertible g b a) => f -> g
	compose :: forall a b c. forall g h. (Invertible f a b, Invertible g b c, Invertible h a c) => f -> g -> h

data InvFunc a b = Inv (a->b) (b->a)

instance forall a b. Invertible (InvFunc a b) a b where
	apply (Inv f g) x = f x
	invert (Inv x y) = (Inv y x)
	compose (Inv x1 y1) (Inv x2 y2) = (Inv (x2.x1) (y1.y2))

