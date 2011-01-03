module Functions.Slerp where

import Data.VectorSpace

slerp p0 p1 t =
     (   sin ((1-t) * omega) *^ p0 
     ^+^ sin (  t   * omega) *^ p1
        ) ^/ sin omega
    where
        omega = acos (clamp (-1) 1 (normalized p0 <.> normalized p1))

clamp lo hi = max lo . min hi