-- "hyper-trees" / "2^k-trees": a generalization of b-trees, quadtrees, 
-- octrees, etc. to arbitrary dimensions
module htree

import btree
import traversable

data HTree : (k : Nat) -> (n : Nat) -> (t : Set) -> Set where
    Leaf : t -> HTree k n t
    Node : BTree k (HTree k n t) -> HTree k (S n) t

isLeaf : HTree k n t -> Maybe t
isLeaf (Leaf t) = Just t
isLeaf _        = Nothing

isNode : HTree k (S n) t -> Maybe (BTree k (HTree k n t))
isNode (Node t) = Just t
isNode _        = Nothing

hTree_eq_with : (a -> b -> Bool) -> HTree k n a -> HTree k n b -> Bool
hTree_eq_with eq (Leaf x) (Leaf y)    = eq x y
hTree_eq_with eq (Node x) (Node y)    = bTree_eq_with (hTree_eq_with eq) x y
hTree_eq_with _ _ _ = False

instance Eq t => Eq (HTree k n t) where
    (==) = hTree_eq_with (==)

instance Functor (HTree k n) where
    fmap f (Leaf x) = Leaf (f x)
    fmap f (Node t) = Node (fmap (fmap f) t)

instance Applicative (HTree k n) where
    pure = Leaf
    (Leaf f)  <$> xs         = fmap f xs
    fs        <$> (Leaf x)   = fmap (\f => f x) fs
    (Node fs) <$> (Node xs)  = Node [| fs <$> xs |]

-- the typechecker needs a little help with "traverse isLeaf" in "pack"
allLeaves : Traversable T => T (HTree k n t) -> Maybe (T t)
allLeaves = traverse isLeaf

pack : Eq t => HTree k n t -> HTree k n t
pack (Leaf x) = Leaf x
pack (Node t) = 
    let packed_t = fmap pack t
     in case allLeaves packed_t of
        (Just ls)   => case isUniform ls of
            (Just l)    => Leaf l
            Nothing     => Node packed_t
        Nothing     => Node packed_t

index : Vect (Vect Bool k) n -> HTree k n t -> t
index Nil (Leaf y) = y
index (x :: xs) (Leaf y) = y
index (x :: xs) (Node t) = index xs (index x t)
