import Data.Tree.NTree.TypeDefs
import Text.XML.HXT.Arrow

pruneTree :: ((NTree a) -> Bool) -> (NTree a) -> [NTree a]
pruneTree f tree =
	if (f tree)
		then [NTree node (concatMap (pruneTree f) children)]
		else []
			where (NTree node children) = tree

pruneRoot f tree = NTree node (concatMap (pruneTree f) children)
			where (NTree node children) = tree

filterXml filter src dst = do 
	runX (readDocument [(a_validate, v_0)] src >>> filter >>> writeDocument [] dst )
	return ()

pruneXml f src dst = filterXml (arr pruneRoot f) src dst

