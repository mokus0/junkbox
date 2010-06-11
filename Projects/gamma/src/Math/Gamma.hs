module Math.Gamma where

import Math.Gamma.Stirling (lnGammaStirling)
import qualified Data.Vector.Unboxed as V

import qualified NR.Ch6.S2 as NR

-- |Gamma function.  Minimal definition is ether gamma or lnGamma.
class Floating a => Gamma a where
    -- |The gamma function:  gamma z == integral from 0 to infinity of
    -- @\t -> t**(z-1) * exp (negate t)@
    gamma :: a -> a
    gamma z = exp (lnGamma z)
    
    -- |Natural log of the gamma function
    lnGamma :: a -> a
    lnGamma z = log (gamma z)
    
    -- |Natural log of the factorial function
    lnFactorial :: Integral b => b -> a
    lnFactorial n = lnGamma (fromIntegral n+1)

instance Gamma Float where
    lnGamma = realToFrac . (lnGamma :: Double -> Double) . realToFrac

instance Gamma Double where
    lnGamma z
        | z <= 0    = error "lnGamma: z <= 0"
        | otherwise = lnGammaStirling cs z
            where
                cs = [8.333333333333333e-2,8.333333333333333e-2,0.16388888888888886,0.4833333333333333,1.903571428571429,9.386904761904761,55.627182539682536,385.06111111111113,3049.370286195286,27190.662878787887,269592.44589993346,2942145.3460622714,3.50467952851801e7,4.524846280013889e8,6.294024232904708e9,9.38382340807317e10,1.492815245447996e12,2.524017944507227e13,4.519816875674298e14,8.54550564657986e15,1.701096502429735e17,3.5563045547165394e18,7.790305587568763e19,1.7843945212560584e21,4.265604690290731e22,1.0623408958587036e24,2.751951339532148e25,7.403960785750008e26,2.066018059231738e28,5.971632845044214e29,1.7857526486045162e31,5.5186535788731415e32,1.7606535181259371e34,5.793179130771491e35,1.9640891488262395e37,6.855336982941969e38,2.4612930067367653e40,9.082968882150484e41,3.442733780636743e43,1.3393308411812385e45,5.344340964125772e46,2.185996947188644e48,9.159978731407522e49,3.9299005151333595e51,1.7253407292374263e53,7.747277185612858e54,3.556221589576943e56,1.667968761559557e58,7.9900401948555745e59,3.907353447813614e61,1.949878294833781e63,9.925421986966868e64,5.1515687928560355e66,2.7253221848195973e68,1.4690238165846874e70,8.065345516211638e71,4.5087503092906074e73,2.5656031618944537e75,1.485554985434695e77,8.750329116748262e78,5.241689238456796e80,3.1923341021981884e82,1.976143574771177e84,1.2430494266586727e86,7.943433739223252e87,5.155511655407994e89,3.3976276077190684e91,2.273109127596193e93,1.5435057942813756e95,1.0635196363731752e97,7.434307966758984e98,5.27113917430575e100,3.790102215163827e102,2.763095252574831e104,2.0420085889784585e106,1.5295247952489544e108,1.1609548056546692e110,8.92808983234751e111,6.955250793300708e113,5.487903618729698e115,4.3850023423111934e117,3.547601482243338e119,2.905595213529364e121,2.4088287912463064e123,2.021082765539389e125,1.7159629631013243e127,1.4740666287887255e129,1.2810109854334963e131,1.1260498522436849e133,1.0010947776338424e135,9.000168391918157e136,8.181448460882842e138,7.519021489775627e140,6.985421124021698e142,6.559544427400809e144,6.225228884669792e146,5.970205876469156e148,5.785333587102152e150,5.664040693877057e152,5.601932500205362e154,5.596525976326634e156,5.64709123983705e158,5.754585604247848e160,5.921673371976321e162,6.152830761715269e164,6.45454133245942e166,6.835593542580309e168,7.307499220729144e170,7.885060359024528e172,8.587122543554633e174,9.437567514644513e176,1.0466616137905457e179,1.1712538291694508e181,1.32239003587719e183,1.506252766536368e185,1.7307423274977904e187,2.0059972984932363e189,2.345088907711124e191,2.7649516435228693e193,3.287636424383647e195,3.942006366791424e197,4.766042863914502e199,5.80999740587956e201,7.140721218659755e203,8.847643381674349e205,1.1051067747027498e208,1.3913748020686558e210,1.765712079334752e212,2.25841908035145e214,2.911196521034469e216,3.781766557906259e218,4.950492157574019e220,6.529909416896952e222,8.67852849040609e224,1.1620922318590026e227,1.5677125945811183e229,2.1305894685194473e231,2.9168699649534075e233,4.022491656930946e235,5.587418328222332e237,7.817045974533869e239,1.1014566174169378e242,1.5630162846163608e244,2.2336206523507213e246,3.2142812099674577e248,4.657639684255855e250,6.795709230011592e252,9.983209083701882e254,1.4765627177896966e257,2.1986703974178737e259,3.2959094388828123e261,4.973681759443077e263,7.555256734039819e265,1.155234491562628e268,1.7779607848533238e270,2.754146150466993e272,4.2938459686083e274,6.737251261048644e276,1.0638445928496377e279,1.6905007999230683e281,2.703193372409347e283,4.3495708017701007e285,7.042169547942422e287,1.1472042891111797e290,1.8803248847853173e292,3.100749660146654e294,5.144299246467009e296,8.586094638198517e298,1.441648736184445e301]

    lnFactorial n
        | n' < 0                = error "lnFactorial n: n < 0"
        | n' < toInteger nFacs  = facs V.! fromIntegral n
        | otherwise             = lnGamma (fromIntegral n+1)
        where
            n' = toInteger n
            nFacs       = 2000 -- limited only by time and space
            facs        = V.map lnGamma (V.enumFromN 1 nFacs)

-- |Incomplete gamma functions.  Minimal definition is either 'p' or 'q', preferably both.
class Gamma a => IncGamma a where
    -- |Lower gamma function: lowerGamma s x == integral from 0 to x of 
    -- @\t -> t**(s-1) * exp (negate t)@
    lowerGamma :: a -> a -> a
    lowerGamma s x = exp (lnLowerGamma s x)
    -- |Upper gamma function: lowerGamma s x == integral from x to infinity of 
    -- @\t -> t**(s-1) * exp (negate t)@
    upperGamma :: a -> a -> a
    upperGamma s x = exp (lnUpperGamma s x)
    
    -- |Natural log of lower gamma function
    lnLowerGamma :: a -> a -> a 
    lnLowerGamma s x = lnGamma s + log (p s x)
    -- |Natural log of upper gamma function
    lnUpperGamma :: a -> a -> a
    lnUpperGamma s x = lnGamma s + log (q s x)
    
    -- |Regularized lower incomplete gamma function: lowerGamma z / gamma z
    p :: a -> a -> a
    p s x = 1 - q s x
    -- |Regularized upper incomplete gamma function: upperGamma z / gamma z
    q :: a -> a -> a
    q s x = 1 - p s x

instance IncGamma Float where
    p s x = realToFrac $ (NR.gammp :: Double -> Double -> Double) (realToFrac s) (realToFrac x)
    q s x = realToFrac $ (NR.gammq :: Double -> Double -> Double) (realToFrac s) (realToFrac x)
instance IncGamma Double where
    p = NR.gammp
    q = NR.gammq

-- |Factorial function
class Num a => Factorial a where
    factorial :: Integral b => b -> a
    factorial = fromInteger . factorial

instance Factorial Integer where
    factorial n = product [1..toInteger n]

instance Factorial Float where
    factorial = realToFrac . (factorial :: Integral a => a -> Double)
instance Factorial Double where
    factorial n
        | n < 0        = error "factorial: n < 0"
        | n < nFacs    = facs V.! fromIntegral n
        | otherwise     = infinity
        where
            nFacs :: Num a => a
            nFacs       = 171 -- any more is pointless, everything beyond here is "Infinity"
            facs        = V.scanl (*) 1 (V.enumFromN 1 nFacs)
            infinity    = facs V.! nFacs



binomialCoefficient :: (Integral a, Integral b) => a -> a -> b
binomialCoefficient n k
    | n < 0     = error "binomialCoefficient n k: n < 0"
    | k < 0     = error "binomialCoefficient n k: k < 0"
    | k > n     = error "binomialCoefficient n k: k > n"
    | n < 171   = floor (0.5 + factorial n' / (factorial k' * factorial (n'-k')) :: Double)
    | otherwise = floor (0.5 + exp (lnBinomialCoefficient n k)                   :: Double)
    where
        n' = fromIntegral n; k' = fromIntegral k

lnBinomialCoefficient :: (Integral a, Gamma b) => a -> a -> b
lnBinomialCoefficient n k
    | n < 0     = error "lnBinomialCoefficient n k: n < 0"
    | k < 0     = error "lnBinomialCoefficient n k: k < 0"
    | k > n     = error "lnBinomialCoefficient n k: k > n"
    | otherwise = lnFactorial n - lnFactorial k - lnFactorial (n-k)

beta :: Gamma a => a -> a -> a
beta z w = exp (lnGamma z + lnGamma w - lnGamma (z+w))
