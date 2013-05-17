data Quine
    = Data [Either Quine String]
    | Self
    deriving Show

quine self = go self
    where
        go (Data d) = concatMap (either go id) d
        go Self = show self

main = putStrLn . quine $ Data [Right "data Quine\n    = Data [Either Quine String]\n    | Self\n    deriving Show\n\nquine self = go self\n    where\n        go (Data d) = concatMap (either go id) d\n        go Self = show self\n\nmain = putStrLn . quine $ ",Left Self]
