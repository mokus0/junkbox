{-# LANGUAGE BangPatterns #-}
module NR.Ch5.S4 where

trigSeq theta delta = go (cos theta) (sin theta)
    where
        alpha = 2 * sin (delta / 2) ^ 2
        beta  = sin delta
        
        go !cos_theta !sin_theta 
            = (sin_theta, cos_theta)
            : go next_cos next_sin
            where
                next_cos = cos_theta - (alpha * cos_theta + beta * sin_theta)
                next_sin = sin_theta - (alpha * sin_theta - beta * cos_theta)

enumSinFromBy theta delta = map fst (trigSeq theta delta)
enumCosFromBy theta delta = map snd (trigSeq theta delta)
enumTanFromBy theta delta = map (uncurry (/)) (trigSeq theta delta)