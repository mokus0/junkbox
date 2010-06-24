{-# LANGUAGE BangPatterns #-}
module Math.Sequence.Trig where

-- |Efficient recurrence for cosine and sine sequences with regularly-spaced
-- parameters.  @trigSeq theta delta@ === @zip (map sin thetas) (map cos thetas)@
-- where @thetas = iterate (+delta) theta@.
trigSeq :: Floating a => a -> a -> [(a,a)]
trigSeq theta delta = go (sin theta) (cos theta)
    where
        alpha = 2 * sin (delta / 2) ^ 2
        beta  = sin delta
        
        go !sin_theta !cos_theta
            = (sin_theta, cos_theta)
            : go next_sin next_cos
            where
                next_sin = sin_theta - (alpha * sin_theta - beta * cos_theta)
                next_cos = cos_theta - (alpha * cos_theta + beta * sin_theta)

-- |@enumSinFromBy theta delta@ === @map sin (iterate (+delta) theta)@
enumSinFromBy :: (Floating b) => b -> b -> [b]
enumSinFromBy theta delta = map fst (trigSeq theta delta)
-- |@enumCosFromBy theta delta@ === @map cos (iterate (+delta) theta)@
enumCosFromBy :: (Floating b) => b -> b -> [b]
enumCosFromBy theta delta = map snd (trigSeq theta delta)
-- |@enumTanFromBy theta delta@ === @map tan (iterate (+delta) theta)@
enumTanFromBy :: (Floating b) => b -> b -> [b]
enumTanFromBy theta delta = map (uncurry (/)) (trigSeq theta delta)