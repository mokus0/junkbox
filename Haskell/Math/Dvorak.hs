module Math.Dvorak where

import Control.Applicative
import Data.List
import Data.Maybe
import Data.Ord
import qualified Data.Permute as P
import qualified Data.Vector.Unboxed as U

-- Parallel listings of the parts of the dvorak/qwerty keymaps that differ
dv = U.fromList "[]',.pyfgcrl/=oeuidhtns-;qjkxbwvz"
qw = U.fromList "-=qwertyuiop[]sdfghjkl;'zxcvbn,./"

-- the permutation taking dv to qw (and its inverse)
qwToDv = orderVec qw `andThen` scrambleVec dv
dvToQw = P.inverse qwToDv

sanityCheck@True = and
    [ U.length dv == U.length qw
    , permute dvToQw dv == qw
    , permute qwToDv qw == dv
    ]

-- cycles:
qwToDvCycles = map (map (U.zip qw dv U.!)) (P.cycles dvToQw)

pprCycles cs = sequence_ (intersperse (putStrLn "") (map pprCycle (sortBy (comparing length) cs)))
pprCycle cyc = do
    putStrLn ("Period: " ++ show (length cyc))
    sequence_
        [ putStrLn $ unwords [[a], "->", [b]]
        | (a, b) <- cyc
        ]

main = do
    pprCycles qwToDvCycles
    putStrLn ""
    putStrLn ("Total period: " ++ show (P.period qwToDv))


-- * missing permutation functions:

-- apply a permutation to a vector
permute p v = U.generate (U.length v) (\i -> v U.! (p `P.at` i))

-- construct a permutation that applies 'p' and then applies 'q'.
-- 'p' and 'q' must be permutations of the same size.
andThen p q = P.listPermute (P.size p) [ p `P.at` i | i <- P.elems q]

-- compute the permutation that would rearrange an unboxed vector into ascending order
-- (P.order on a vector)
orderVec = P.order <$> U.length <*> U.toList

-- compute the permutation that world scramble a sorted vector into the given vector
-- (P.rank on a vector)
scrambleVec = P.rank <$> U.length <*> U.toList
