{-
 -      ``latitude''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -}
{-# LANGUAGE
        ImplicitParams
  #-}

module Math.Latitude where

wgs84lat4m :: RealFloat a => a -> a -> a -> a
wgs84lat4m = let ?epsilon = 4 in wgs84lat

wgs84lat :: (RealFloat a, ?epsilon :: a) => a -> a -> a -> a
wgs84lat = let  ?a = 6378137 
                ?b = 6356752.314245
           in   lat

lat :: ( RealFloat a
       , ?a :: a                -- radius at axis
       , ?b :: a                -- radius at equator
       , ?epsilon :: a          -- required tightness (in units at equator)
       ) => a -> a -> a -> a
lat x y z = converge (iterate improve phi)
        where   -- converge : seek through a converging sequence until tight enough
                converge (a : rest@(b : _))
                        | a ~= b        = b
                        | otherwise     = converge rest
                        where   x ~= y = abs (x - y) < epsilon
                                epsilon = atan2 ?epsilon ?a
                
                -- e_2 : ellipsoid eccentricity squared
                e_2 = (?a * ?a - ?b * ?b) / (?a * ?a)
                
                -- p : distance from z axis
                p = sqrt(x*x + y*y)
                
                -- phi, improve : initial estimate and refinement step
                phi = atan2 z (p * (1 - e_2))
                improve phi = atan2 (z + v * s) p
                        where 
                                s = sin phi
                                v = (?a * e_2) / sqrt (1 - e_2 * (s * s))
