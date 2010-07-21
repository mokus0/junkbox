module Math.DeCasteljau where

import Data.VectorSpace

interp a x y = (1-a) *^ x ^+^ a *^ y

deCasteljau t [p] = [[p]]
deCasteljau t ps = ps : deCasteljau t (zipWith (interp t) ps (tail ps))

bezier t ps = head $ last $ deCasteljau t ps

-- note; 2nd curve has parameter reversed
split t ps =
  let pss = deCasteljau t ps
  in (map head pss, map last pss)
