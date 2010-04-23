{-# LANGUAGE
        RecordWildCards, FlexibleContexts
  #-}
module NR.Ch3.S1
    ( Interp(..), interp, interps
    , InterpHint, noHint, interp'
    , linearInterp
    ) where

import qualified Data.Vector as V
import qualified Data.Vector.Unboxed as UV
import qualified Data.Vector.Generic as GV
import Data.Bits
import Control.Monad.State

fromList xys interpOrder interpRaw = Interp
    { interpTable   = V.fromList xys, ..}

data Interp v a b c = Interp
    { interpTable   :: v (a, b)
    , interpOrder   :: Int
    , interpRaw     :: v (a, b) -> Int -> a -> c
    -- ^ the raw interpolation function will be passed a pre-sliced portion
    -- of the table, with x centered as well as possible in the slice.  In
    -- case the method makes use of any auxiliary tables, the index of the 
    -- first element of the slice is passed as well.
    }

-- |This is an opaque type whose values are sort of irrelevant.  Basically,
-- it is used to choose between 2 methods of locating the portion of the table
-- to be used, both of which return the same results.  The only difference is 
-- efficiency.  Thus, some rewrite rules are in place that, strictly speaking,
-- are not valid because they change the InterpHint that will be returned.
-- However, this is considered (by me) completely safe because the contents
-- of an InterpHint are unobservable outside this module.
data InterpHint a = InterpHint
    { interpHintJSav    :: Maybe Int
    , interpHintCor     :: Bool
    }

noHint = InterpHint Nothing False

{-# INLINE interp' #-}
interp' Interp{..} hint@InterpHint{..} x = (interpRaw vec j x, newHint)
    where
        vec = GV.slice j interpOrder interpTable
        (j, newHint) 
            | interpHintCor = hunt   interpTable interpOrder hint x
            | otherwise     = locate interpTable interpOrder hint x

interpState :: (GV.Vector v (a,b), Ord a) => Interp v a b c -> a -> State (InterpHint a) c
interpState f x = do
    hint <- get
    let (y, newHint) = interp' f hint x
    put newHint
    return y

interp :: (GV.Vector v (a,b), Ord a) => Interp v a b c -> a -> c
interp f x = fst (interp' f noHint x)
interps :: (GV.Vector v (a,b), Ord a) => Interp v a b c -> [a] -> [c]
interps f xs = evalState (mapM (interpState f) xs) noHint

{-# RULES
    "map interp"    forall f xs. map (interp f) xs = interps f xs
  #-}

-- Given a value x, locate a value j such that x is (insofar as possible) 
-- centered in the subrange table[j .. j+order-1].  The 'fst' values in table
-- must be monotonically increasing.  
-- The returned value is not less than 0 nor greater than n-1.
locate table order hint x = bisect table order hint n x 0 (n-1)
    where
        n = GV.length table

-- Given a value x, locate a value j such that x is (insofar as possible) 
-- centered in the subrange table[j .. j+order-1].  The 'fst' values in table
-- must be monotonically increasing.  
-- The returned value is not less than 0 nor greater than n-1.
--
-- Invariant: not called unless the hint has a saved 'j' value
hunt table order hint@InterpHint{..} x
    | jsav < 0 || jsav  >= n        = locate table order hint x
    | x >= fst (table GV.! jsav)    = huntUp   jsav 1
    | otherwise                     = huntDown jsav 1
    where
        n = GV.length table
        Just jsav = interpHintJSav
        
        huntUp   jl inc
            | ju >= n                   = bisect table order hint n x jl (n-1)
            | x <  fst (table GV.! ju)  = bisect table order hint n x jl ju 
            | otherwise                 = huntUp   ju (inc+inc)
            where ju = jl + inc
            
        huntDown ju inc
            | jl <= 0                   = bisect table order hint n x 0 ju
            | x >= fst (table GV.! jl)  = bisect table order hint n x jl ju
            | otherwise                 = huntDown jl (inc+inc)
            where jl = ju - inc

{-# INLINE bisect #-}
bisect table order hint n x = go
    where
        go jl ju
            | ju - jl <= 1  =
                let j = max 0 (min (n-order) (jl - ((order - 2) `shiftR` 1)))
                 in (j, updateHint order hint jl)
            | otherwise     =
                let jm = (ju+jl) `shiftR` 1
                 in if x > fst (table GV.! jm)
                        then go jm ju
                        else go jl jm

get_dj n = 1
-- This is what was in NR, but it seems rather insane because it always returns 1 for
-- every positive number.
-- get_dj = memo $ \n -> min 1 (truncate (realToFrac n ** 0.25))

memo f = \x -> func x
    where
        b = 10
        n = bit b
        tables = [UV.generate n $ \j -> f ((i*n) + j) | i <- [0,n..]]
        
        mask = n - 1
        func x = tables !! i UV.! j
            where
                i = x `shiftR` b
                j = x .&. mask


updateHint order hint j = InterpHint
    { interpHintJSav    = Just j
    , interpHintCor     = case interpHintJSav hint of
        Nothing     -> False
        Just jsav   -> abs (j - jsav) <= (get_dj order)
    }

linearInterp table
    | GV.length table < 2   = error "linearInterp: 2 or more data points required"
    | otherwise             = Interp table 2 rawInterp
    where
        rawInterp v j x
            | x0 == x1  = y0
            | otherwise = y0 + (x-x0) / (x1-x0) * (y1-y0)
            where
                [(x0,y0), (x1,y1)] = GV.toList v