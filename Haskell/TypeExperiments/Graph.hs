module TypeExperiments.Graph where


data Graph n =
	  Graph [n] [(Int,Int)]
	deriving (Show, Read)

data Graph2 n = Graph2 [n] ((Int,n) -> (Int,n) -> Bool)

flatten (Graph2 nodes rule) = Graph nodes [(x,y) | x <- [0..(nodeCount - 1)], y <- [0 .. (nodeCount - 1)], x /= y, connected x y]
	where
		nodeCount = length nodes
		connected x y = rule (x, nodes!!x) (y, nodes!!y)
