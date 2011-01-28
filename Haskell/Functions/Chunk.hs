module Functions.Chunk where

chunk _        [] = []
chunk []       xs = [xs]
chunk (sz:szs) xs = ys : chunk szs zs
    where
        ~(ys,zs) = splitAt sz xs

chunkBy _  [] = []
chunkBy sz xs = ys : chunkBy sz zs
    where
        ~(ys,zs) = splitAt (sz xs) xs