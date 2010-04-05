module Math.ScaleFree where

import Control.Applicative
import Data.Graph.Inductive
import Data.List
import qualified Data.Map as M
import Data.Maybe
import Data.Random

-- |Extend the seed graph by n nodes, using the Barabási-Albert 
-- preferential-attachment algorithm.  Any nodes in the seed without any links
-- to them have 0 chance of gaining links, so the minimal seed graph is
-- @insEdge (1, 2, ()) (insNodes [(1, ()), (2,())] empty)@
barabásiAlbert seed n = go n firstAvailableNode (genericLength (edges seed)) seed
    where
        firstAvailableNode = 1 + maximum (-1 : nodes seed)
        
        go 0 _ _ gr = return gr
        go (n+1) m k gr = do
            links <- catMaybes <$> sequence
                [ do
                    accept <- bernoulli p
                    if accept
                        then return (Just ((), node))
                        else return Nothing
                | node <- nodes gr
                , let p :: Float
                      p = fromIntegral (deg gr node) / k
                ]
            
            let newNode = ([], m, (), links)
            go n (m+1) (k + genericLength links) (newNode & gr)

-- doesn't generate a scale-free network, but this is a pretty standard algorithm.
-- Generates a graph with n nodes, where every pair of nodes has a @p@ probability
-- of being directly connected.
erdősRényi directed n p = do
    let nodes = [1..n]
    let mkEdges = M.fromList <$> sequence
            [ do
                nEdges <- binomial bCount p
                bs <- shuffleNofM nEdges bCount bs
                return (a,bs)
            | a <- nodes
            , let bs = [a+1 .. n]
                  bCount = n - a
            ]
    edges <- mkEdges
    revEdges <- if directed then mkEdges else return edges
        
    
    return $ buildGr
        [ (revLinks, n, (), links)
        | n <- nodes
        , let links    = zip (repeat ()) (M.findWithDefault [] n edges)
              revLinks = zip (repeat ()) (M.findWithDefault [] n revEdges)
        ]
    -- return (insEdges edges (insNodes lNodes Data.Graph.Inductive.empty))

