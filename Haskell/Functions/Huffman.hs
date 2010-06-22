module Functions.Huffman where

import Control.Arrow
import qualified Data.Map as M
import Data.List

data Tree a
    = Leaf a
    | Node (Tree a) (Tree a)
    deriving (Eq, Ord, Show)

analyze alphabet sample = map (snd &&& fst) (M.toList allFreqs)
    where
        baseFreqs   = M.fromListWith (+) [(sym,0) | sym <- alphabet]
        sampleFreqs = M.fromListWith (+) [(sym,1) | sym <- sample]
        allFreqs = M.unionWith (+) baseFreqs sampleFreqs

huffmanCode allFreqs = (encode tree, decode tree)
    where
        tree = huffmanTree allFreqs

huffmanTree allFreqs = tree
    where
        tree = collect . sort . map (id *** Leaf) $ allFreqs
        
        collect [(w, t)] = t
        collect ((pa, a):(pb,b):rest) = 
            collect (insert (pa + pb, Node a b) rest)
        
code (Leaf sym) = [(sym, [])]
code (Node l r) = 
    [ (sym, pref:str)
    | (pref, sub) <- [('0',l), ('1',r)]
    , (sym, str) <- code sub
    ]

encode tree = concatMap (tbl M.!)
    where
        tbl = M.fromList (code tree)

decode tree = go tree
    where
        go (Leaf sym) [] = sym : []
        go (Leaf sym) xs = sym : go tree xs
        go (Node a b) ('0':xs) = go a xs
        go (Node a b) ('1':xs) = go b xs
        go _ xs = error ("decoding error at " ++ show xs)
