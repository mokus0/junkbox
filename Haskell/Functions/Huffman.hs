module Functions.Huffman where

import Control.Arrow
import Data.List
import qualified Data.Map as M
import Data.Ord

data Tree a
    = Leaf a
    | Node (Tree a) (Tree a)
    deriving (Eq, Ord, Show)

analyze alphabet baseFreq sample = map (snd &&& fst) (M.toList allFreqs)
    where
        baseFreqs   = M.fromListWith (+) [(sym,baseFreq) | sym <- alphabet]
        sampleFreqs = M.fromListWith (+) [(sym,1)        | sym <- sample]
        allFreqs = M.unionWith (+) baseFreqs sampleFreqs

huffmanCode allFreqs = (encode tree, decode tree)
    where
        tree = huffmanTree allFreqs

huffmanTree :: (Num a, Ord a) => [(a,b)] -> Tree b
huffmanTree = collect . sortBy (comparing fst) . map (id *** Leaf)
    where
        collect [] = error "Why are you even calling this function?  Your symbol table is empty."
        collect [(w, t)] = t
        collect ((pa, a):(pb,b):rest) = 
            collect (insertBy (comparing fst) (pa + pb, Node a b) rest)
        
code (Leaf sym) = [(sym, [])]
code (Node l r) = 
    [ (symbol, prefix:codeWord)
    | (prefix, subTree) <- [('0',l), ('1',r)]
    , (symbol, codeWord) <- code subTree
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
