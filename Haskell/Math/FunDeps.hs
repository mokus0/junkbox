module Math.FunDeps where

import Control.Monad
import Data.List
import Data.Graph.Inductive

type TyVar = Char
type FunDep = Gr TyVar ()

fundeps_equal :: FunDep -> FunDep -> Bool
fundeps_equal fd1 fd2 = reachables fd1 == reachables fd2
--    = nodes fd1 == nodes fd2
--    && labEdges (trc fd1) == labEdges (trc fd2)

-- silly brute-force... there's a much simpler and faster inductive algorithm
all_fundeps tyVars = nubBy fundeps_equal
    [ mkGraph (zip [0..] tyVars) [(x,y,()) | (x,y) <- deps]
    | deps <- subsets (distinctPairs (zipWith const [0..] tyVars))
    ]

reachables gr = [(nd, sort (reachable nd gr)) | nd <- sort (nodes gr)]

all_fundeps' [] = []
all_fundeps' [t] = all_fundeps [t]
all_fundeps' (t:ts) = nubBy fundeps_equal $ do
    fd <- all_fundeps' ts
    let tNums = zipWith const [1..] ts
    to  <- subsets tNums
    frm <- subsets tNums
    let newEdgesTo  = [(0, x, ()) | x <-  to]
        newEdgesFrm = [(x, 0, ()) | x <- frm]
        oldEdges    = [(x+1, y+1, ()) | (x,y,~()) <- labEdges fd]
        oldNodes    = map (\(n,t) -> (n+1,t)) (labNodes fd)
    
    return (mkGraph ((0, t):oldNodes) (oldEdges ++ newEdgesTo ++ newEdgesFrm))


pairs xs = liftM2 (,) xs xs
distinctPairs xs = filter (uncurry (/=)) (pairs xs)

subsets [] = [[]]
subsets (x:xs) = do
    hd <- [(x:), id]
    tl <- subsets xs
    return (hd tl)

show_fundep fd = undefined
    
    where 
        tyVars = map snd (labNodes fd)