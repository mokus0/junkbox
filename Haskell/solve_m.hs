module Solve_m where

import Math.Root.Finder.Newton

precis = 1e-10 :: Double

-- original code:
-- 
-- solve_m f ar st qt = solve_m' f ar st qt 4.0
--   where
--     solve_m' f ar st qt mg 
--       | rd > precis    = f' (mg - sf)  
--       | rd < (-precis) = f' (mg + sf)  
--       | otherwise = mg
--         where 
--           f' = solve_m' f ar st qt
--           rt = st + qt   
--           r' = f st ar mg 
--           rd = rt - r'    
--           sf = abs(rd) 

-- static loop parameter elimination:
-- 
-- solve_m f ar st qt = solve_m' 4.0
--   where
--     rt = st + qt   
--     solve_m' mg 
--       | rd > precis    = solve_m' (mg - sf)  
--       | rd < (-precis) = solve_m' (mg + sf)  
--       | otherwise = mg
--         where 
--           rd = rt - f st ar mg 
--           sf = abs(rd) 

-- abstract rd; solve_m seeks a root of rd (considering it as a function of m):
-- 
-- solve_m f ar st qt = solve_m' 4.0
--   where
--     rt = st + qt
--     rd mg = rt - f st ar mg
--     solve_m' mg 
--       | sf > precis    = solve_m' (mg - abs sf)  
--       | sf < (-precis) = solve_m' (mg + abs sf)  
--       | otherwise = mg
--         where 
--           sf = rd mg

-- simplify abs logic:
-- 
-- solve_m f ar st qt = solve_m' 4.0
--   where
--     rt = st + qt
--     rd mg = rt - f st ar mg
--     solve_m' mg 
--       | abs sf > precis = solve_m' (mg - sf)  
--       | otherwise = mg
--         where 
--           sf = rd mg

-- solve_m f ar st qt = solve_m' 4.0
--   where
--     rt = st + qt
--     err mg = rt - f st ar mg
--     solve_m' mg 
--       | abs err_mg > precis = solve_m' (mg - err_mg)  
--       | otherwise = mg
--         where 
--           err_mg = err mg

-- This is just newton's method with the derivative hard-coded as 1:
-- 
solve_m f ar st qt = case newton f' precis 3 5 of
    Left state -> error ("failed to converge:  halted with state: " ++ show state)
    Right m -> m
  where
    f' mg = (st + qt - f st ar mg, 1)


-- alternate version:
-- solve_m precis f ar st qt = solve_m_simpl precis f'
--   where
--     f' m = rt - f st ar m
--     rt = st + qt
-- 
-- solve_m_simpl precis f = convSum accept (iterate improve 0.0)
--   where
--     accept m dm = abs dm < precis
--     improve mg = mg - f mg
--     
--     
--     -- solve_m' mg 
--     --   | abs rd > precis = solve_m' (mg - rd)  
--     --   | otherwise = mg
--     --     where 
--     --       rd = f mg 
-- 
-- convSum accept = go 0 0
--   where
--     go n x (dx:dxs)
--         | accept x dx   = (n, x)
--         | otherwise     = go (n+1) (x + dx) dxs