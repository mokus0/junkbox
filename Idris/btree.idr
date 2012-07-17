-- full btrees with specified height
module btree

import foldable
import traversable

data BTree : (n : Nat) -> (t : Set) -> Set where
    Tip : t                      -> BTree  O    t
    Bin : BTree n t -> BTree n t -> BTree (S n) t

total bTree_eq_with : (a -> b -> Bool) -> BTree n a -> BTree n b -> Bool
bTree_eq_with eq (Tip x)     (Tip y)     = eq x y
bTree_eq_with eq (Bin lx ly) (Bin rx ry) = (bTree_eq_with eq lx rx) && (bTree_eq_with eq ly ry)

instance Eq t => Eq (BTree n t) where
    (==) = bTree_eq_with (==)

instance Functor (BTree n) where
    fmap f (Tip x) = Tip (f x)
    fmap f (Bin l r) = Bin (fmap f l) (fmap f r)

total bTree_pure : (n : Nat) -> t -> BTree n t
bTree_pure  O    x = Tip x
bTree_pure (S n) x = Bin (bTree_pure n x) (bTree_pure n x)

-- ??? how do I convince idris this is total ???
bTree_ap : (n : Nat) -> BTree n (a -> b) -> BTree n a -> BTree n b
bTree_ap  O    (Tip f)     (Tip x)     = Tip (f x)
bTree_ap (S n) (Bin lf rf) (Bin lx rx) = Bin (bTree_ap n lf lx) (bTree_ap n rf rx)

instance Applicative (BTree n) where
    pure  = bTree_pure n
    (<$>) = bTree_ap   n

instance Foldable (BTree n) where
    foldMap f (Tip x) = f x
    foldMap f (Bin l r) = foldMap f l <+> foldMap f r

instance Traversable (BTree n) where
    sequenceA (Tip x) = [| Tip x |]
    sequenceA (Bin l r) = [| Bin (sequenceA l) (sequenceA r) |]

isUniform : Eq t => BTree n t -> Maybe t
isUniform (Tip x) = Just x
isUniform (Bin l r) with (isUniform l)
    | Nothing   = Nothing
    | (Just x)  with (isUniform r)
        | Nothing   = Nothing
        | (Just y)  = if x == y then Just x else Nothing

index : Vect Bool n -> BTree n t -> t
index  Nil          (Tip y)   = y
index (False :: xs) (Bin l r) = index xs l
index (True  :: xs) (Bin l r) = index xs r
