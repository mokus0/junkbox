-- example "Monad" implementation and an overly long proof that it is wrong
module Monads.BTree where

data Tree a
    = Empty
    | Node a (Tree a) (Tree a)
    deriving (Eq, Ord, Read, Show)

instance Functor Tree where
    fmap f Empty = Empty
    fmap f (Node x l r) = Node (f x) (fmap f l) (fmap f r)

joinTree :: Tree (Tree a) -> Tree a
joinTree Empty = Empty
joinTree (Node Empty _ _) = Empty
joinTree (Node (Node x il ir) ol or) 
    = Node x
        (attachLeft  (joinTree ol) il)
        (attachRight (joinTree or) ir)

attachLeft t Empty = t
attachLeft t (Node x l r) = Node x (attachLeft t l) r

{- 
    lemma 1: attachLeft Empty t = t 
    
    attachLeft Empty t
    
    case t of
        Empty -> Empty
        Node x l r -> Node x (attachLeft Empty l) r
        
    {-  
        -- induction hypothesis: 
        attachLeft Empty l = l
        
        -- attachLeft Empty t ==
        case t of
            Empty -> Empty
            Node x l r -> Node x l r
        
        -- attachLeft Empty t ==
        t
     -}
 -}

attachRight t Empty = t
attachRight t (Node x l r) = Node x l (attachRight t r)

{-
    lemma 2: attachRight Empty t = t
    
    -- almost exactly the same proof as lemma 1
 -}

instance Monad Tree where
    return x = Node x Empty Empty
    x >>= f  = joinTree (fmap f x)

-- Monad laws:
-- "Left identity": return a >>= f  ≡  f a
{-
    
    return a >>= f
    
    Node a Empty Empty >>= f
    
    joinTree (fmap f (Node a Empty Empty))
    
    joinTree (Node (f a) Empty Empty)
    
    case f a of
        Empty        -> Empty
        Node x il ir -> Node x
            (attachLeft  (joinTree ol) il)
            (attachRight (joinTree or) ir)
    
    case f a of
        Empty        -> Empty
        Node x il ir -> Node x
            (attachLeft  (joinTree Empty) il)
            (attachRight (joinTree Empty) ir)
    
    case f a of
        Empty        -> Empty
        Node x il ir -> Node x
            (attachLeft  Empty il)
            (attachRight Empty ir)
    
    -- apply lemmas 1 & 2:
    
    case f a of
        Empty        -> Empty
        Node x il ir -> Node x il ir
    
    f a
 -}

-- "Right identity": m >>= return  ≡  m
{-
    x >>= return
    
    joinTree (fmap return x)
    
    joinTree (case x of
        Empty -> Empty
        Node x l r -> Node (return x) (fmap return l) (fmap return r))
    
    case x of 
        Empty -> joinTree Empty
        Node x l r -> joinTree (Node (return x) (fmap return l) (fmap return r)))
    
    case x of 
        Empty -> Empty
        Node x l r -> joinTree (Node (Node x Empty Empty) (fmap return l) (fmap return r)))
    
    case x of 
        Empty -> Empty
        Node x l r -> Node x
            (attachLeft   (joinTree (fmap return l)) Empty) 
            (attachRight  (joinTree (fmap return r)) Empty) 
    
    case x of 
        Empty -> Empty
        Node x l r -> Node x
            (joinTree (fmap return l)) 
            (joinTree (fmap return r)) 
    
    -- by induction:
    case x of 
        Empty -> Empty
        Node x l r -> Node x l r
    
    x
 -}

associativityCounterexample = x
    where
        x = Node y (return (return (return "hi"))) Empty 
        y = Node z (return Empty) Empty 
        z = Node "hi" Empty Empty


-- "Associativity": (m >>= f) >>= g  ≡  m >>= (\x -> f x >>= g)
{-
    -- Proving the equivalent (given that 'x >>= f' = joinTree (fmap f) x)
    -- joinTree . joinTree  ≡  joinTree . fmap joinTree
    
    Counterexample:
        Node (Node (Node _ _ _) b _) a _
        joinTree a = Node _ _ _
        b = Node Empty _ _
        
    
    {-
        -- LHS
        joinTree (joinTree x)
    
        case x of
            Empty           -> joinTree Empty
            Node Empty _ _  -> joinTree Empty
            Node (Node x il ir) ol or ->
                joinTree (Node x
                    (attachLeft  (joinTree ol) il)
                    (attachRight (joinTree or) ir))
    
        case x of
            Empty           -> joinTree Empty
            Node Empty _ _  -> joinTree Empty
            Node (Node y il ir) ol or ->
                joinTree (Node y
                    (attachLeft  (joinTree ol) il)
                    (attachRight (joinTree or) ir))
    
        case x of
            Empty           -> joinTree Empty
            Node Empty _ _  -> joinTree Empty
            Node (Node y il ir) ol or ->
                case y of
                    Empty          -> Empty
                    Node z iil iir ->
                        Node z
                            (attachLeft  (joinTree (attachLeft  (joinTree ol) il)) iil)
                            (attachRight (joinTree (attachRight (joinTree or) ir)) iir)
    
        case x of
            Empty                       -> Empty
            Node Empty _ _              -> Empty
            Node (Node Empty _ _) _ _   -> Empty
            Node (Node (Node z iil iir) il ir) ol or
                -> Node z
                    (attachLeft  (joinTree (attachLeft  (joinTree ol) il)) iil)
                    (attachRight (joinTree (attachRight (joinTree or) ir)) iir)
    
     -}
    {-
        -- RHS
        joinTree (fmap joinTree x)
        
        case x of
            Empty       -> joinTree Empty
            Node y l r  -> 
                joinTree (Node (joinTree y)
                    (fmap joinTree l)
                    (fmap joinTree r))
        
        case x of
            Empty       -> Empty
            Node y l r  -> 
                joinTree (Node (joinTree y)
                    (fmap joinTree l)
                    (fmap joinTree r))
        
        case x of
            Empty       -> Empty
            Node y l r  -> 
                case Node (joinTree y) (fmap joinTree l) (fmap joinTree r) of
                    Empty                       -> joinTree (Node Empty l r)
                    Node Empty _ _              -> joinTree (Node Empty l r)
                    Node (Node z iil iir) il ir -> 
                        Node z 
                            (attachLeft  (joinTree il) iil)
                            (attachRight (joinTree ir) iir)
        
        case x of
            Empty       -> Empty
            Node y l r  -> 
                case joinTree y of
                    Empty          -> Empty
                    Node z iil iir -> 
                        Node z 
                            (attachLeft  (joinTree (fmap joinTree l)) iil)
                            (attachRight (joinTree (fmap joinTree r)) iir)
        
        case x of
            Empty       -> Empty
            Node y l r  -> 
                case (case y of
                    Empty -> Empty
                    Node Empty _ _ -> Empty
                    Node (Node z iil iir) il ir -> 
                        Node z 
                        (attachLeft  (joinTree il) iil)
                        (attachRight (joinTree ir) iir)
                    ) of
                        Empty          -> Empty
                        Node z iil iir -> 
                            Node z 
                                (attachLeft  (joinTree (fmap joinTree l)) iil)
                                (attachRight (joinTree (fmap joinTree r)) iir)
        
        case x of
            Empty       -> Empty
            Node y l r  -> 
                case y of
                    Empty -> Empty
                    Node Empty _ _ -> Empty
                    Node (Node z iil iir) il ir -> 
                        Node z 
                            (attachLeft  (joinTree (fmap joinTree l)) (attachLeft  (joinTree il) iil))
                            (attachRight (joinTree (fmap joinTree r)) (attachRight (joinTree ir) iir))
        
        case x of
            Empty                       -> Empty
            Node Empty _ _              -> Empty
            Node (Node Empty _ _) _ _   -> Empty
            Node (Node (Node z iil iir) il ir) ol or ->
                Node z 
                    (attachLeft  (joinTree (fmap joinTree ol)) (attachLeft  (joinTree il) iil))
                    (attachRight (joinTree (fmap joinTree or)) (attachRight (joinTree ir) iir))
        
        -- by induction:
        case x of
            Empty                       -> Empty
            Node Empty _ _              -> Empty
            Node (Node Empty _ _) _ _   -> Empty
            Node (Node (Node z iil iir) il ir) ol or ->
                Node z 
                    (attachLeft  (joinTree (joinTree ol)) (attachLeft  (joinTree il) iil))
                    (attachRight (joinTree (joinTree or)) (attachRight (joinTree ir) iir))
        
     -}
     
     {-
        Subgoal ('attachRight' version should be provable iff this is):
            attachLeft (joinTree a) (attachLeft (joinTree b) c)
                ≡
            attachLeft (joinTree (attachLeft a b)) c
            
            Counterexample:
                a = Node _ _ _
                b = Node Empty _ _
                c = _
            
            {- 
                -- LHS
                
                attachLeft (joinTree a) (attachLeft (joinTree b) c)
                
                case attachLeft (joinTree b) c of
                    Empty       -> joinTree a
                    Node x l r  -> Node x (attachLeft (joinTree a) l) r
                
                case (case c of
                    Empty           -> joinTree b
                    Node y il ir    -> Node y (attachLeft (joinTree b) il) ir
                    ) of
                        Empty       -> joinTree a
                        Node x l r  -> Node x (attachLeft (joinTree a) l) r
                
                case c of
                    Empty           -> case joinTree b of
                        Empty       -> joinTree a
                        Node x l r  -> Node x (attachLeft (joinTree a) l) r
                    Node y il ir    -> case Node y (attachLeft (joinTree b) il) ir of
                        Empty       -> joinTree a
                        Node x l r  -> Node x (attachLeft (joinTree a) l) r
                
                case c of
                    Empty           -> case joinTree b of
                        Empty       -> joinTree a
                        Node x l r  -> Node x (attachLeft (joinTree a) l) r
                    Node y il ir    -> 
                        let x = y; l = attachLeft (joinTree b) il; r = ir
                         in Node x (attachLeft (joinTree a) l) r
                
                case c of
                    Empty           -> case joinTree b of
                        Empty       -> joinTree a
                        Node x l r  -> Node x (attachLeft (joinTree a) l) r
                    Node x l r      -> 
                         Node x (attachLeft (joinTree a) (attachLeft (joinTree b) l)) r
                
                case c of
                    Empty           -> case joinTree b of
                        Empty       -> attachLeft (joinTree a) Empty
                        Node x l r  -> attachLeft (joinTree a) (Node x l r)
                    Node x l r      -> 
                        Node x (attachLeft (joinTree a) (attachLeft (joinTree b) l)) r
                
                case c of
                    Empty           -> attachLeft (joinTree a) (joinTree b)
                    Node x l r      -> 
                        Node x (attachLeft (joinTree a) (attachLeft (joinTree b) l)) r
                
                -- by induction on subgoal:
                
                case c of
                    Empty           -> attachLeft (joinTree a) (joinTree b)
                    Node x l r      -> 
                        Node x (attachLeft (joinTree (attachLeft a b)) l) r
                    
             -}
            {-
                -- RHS
                
                attachLeft (joinTree (attachLeft a b)) c
                
                case c of
                    Empty       -> joinTree (attachLeft a b)
                    Node x l r  -> Node x (attachLeft (joinTree (attachLeft a b)) l) r
             -}
            {-
                Sub-subgoal:
                    attachLeft (joinTree a) (joinTree b)
                        ≡
                    joinTree (attachLeft a b)
                
                Counterexample:
                    a = Node _ _ _
                    b = Node Empty _ _
                
                {-
                    -- LHS:
                        attachLeft (joinTree a) (joinTree b)
                        
                        case joinTree b of
                            Empty       -> joinTree a
                            Node x l r  -> Node x (attachLeft (joinTree a) l) r
                        
                        case (case b of
                            Empty                       -> Empty
                            Node Empty _ _              -> Empty
                            Node (Node y il ir) ol or   ->
                                Node y
                                    (attachLeft  (joinTree ol) il)
                                    (attachRight (joinTree or) ir)
                            ) of
                                Empty       -> joinTree a
                                Node x l r  -> Node x (attachLeft (joinTree a) l) r
                        
                        case b of
                            Empty                       -> case Empty of
                                Empty       -> joinTree a
                                Node x l r  -> Node x (attachLeft (joinTree a) l) r
                                
                            Node Empty _ _              -> case Empty of
                                Empty       -> joinTree a
                                Node x l r  -> Node x (attachLeft (joinTree a) l) r
                                
                            Node (Node y il ir) ol or   ->
                                case Node y (attachLeft  (joinTree ol) il) (attachRight (joinTree or) ir) of
                                    Empty       -> joinTree a
                                    Node x l r  -> Node x (attachLeft (joinTree a) l) r
                        
                        case b of
                            Empty           -> joinTree a
                            Node Empty _ _  -> joinTree a
                            Node (Node y il ir) ol or   ->
                                let Node x l r = Node y (attachLeft  (joinTree ol) il) (attachRight (joinTree or) ir)
                                in Node x (attachLeft (joinTree a) l) r
                        
                        case b of
                            Empty           -> joinTree a
                            Node Empty _ _  -> joinTree a
                            Node (Node x il ir) ol or   ->
                                Node x
                                    (attachLeft (joinTree a) (attachLeft  (joinTree ol) il))
                                    (attachRight (joinTree or) ir)
                 -}
                {-
                    -- RHS:
                        joinTree (attachLeft a b)
                        
                        case attachLeft a b of
                            Empty                       -> Empty
                            Node Empty _ _              -> Empty
                            Node (Node x il ir) ol or   ->
                                Node x
                                    (attachLeft  (joinTree ol) il)
                                    (attachRight (joinTree or) ir)
                        
                        case (case b of
                            Empty -> a
                            Node x l r -> Node x (attachLeft a l) r
                            ) of
                                Empty                       -> Empty
                                Node Empty _ _              -> Empty
                                Node (Node x il ir) ol or   ->
                                    Node x
                                        (attachLeft  (joinTree ol) il)
                                        (attachRight (joinTree or) ir)
                        
                        case b of
                            Empty -> case a of
                                Empty                       -> Empty
                                Node Empty _ _              -> Empty
                                Node (Node x il ir) ol or   ->
                                    Node x
                                        (attachLeft  (joinTree ol) il)
                                        (attachRight (joinTree or) ir)
                            Node x l r -> case Node x (attachLeft a l) r of
                                Node Empty _ _              -> Empty
                                Node (Node x il ir) ol or   ->
                                    Node x
                                        (attachLeft  (joinTree ol) il)
                                        (attachRight (joinTree or) ir)
                        
                        case b of
                            Empty -> case a of
                                Empty                       -> Empty
                                Node Empty _ _              -> Empty
                                Node (Node x il ir) ol or   ->
                                    Node x
                                        (attachLeft  (joinTree ol) il)
                                        (attachRight (joinTree or) ir)
                            Node x l r -> case x of
                                Empty           -> Empty
                                Node x il ir    ->
                                    Node x
                                        (attachLeft  (joinTree (attachLeft a l)) il)
                                        (attachRight (joinTree r) ir)
                        
                        case b of
                            Empty -> case a of
                                Empty                       -> Empty
                                Node Empty _ _              -> Empty
                                Node (Node x il ir) ol or   ->
                                    Node x
                                        (attachLeft  (joinTree ol) il)
                                        (attachRight (joinTree or) ir)
                            Node Empty _ _  -> Empty
                            Node (Node y il ir) ol or -> 
                                Node y
                                    (attachLeft  (joinTree (attachLeft a ol)) il)
                                    (attachRight (joinTree or) ir)
                 -}
             -}
      -}
 -}