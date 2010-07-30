> {-# LANGUAGE TypeFamilies #-}
> module Math.DeBoor where
> import Data.List
> import Data.VectorSpace

Andrew Coppin wrote:
| Given a suitable definition for Vector2 (i.e., a 2D vector with the 
| appropriate classes), it is delightfully trivial to implement de 
| Casteljau's algorithm:
| 
| de_Casteljau :: Scalar -> [Vector2] -> [[Vector2]]
| de_Casteljau t [p] = [[p]]
| de_Casteljau t ps = ps : de_Casteljau t (zipWith (line t) ps (tail ps))
| 
| line :: Scalar -> Vector2 -> Vector2 -> Vector2
| line t a b = (1-t) *| a + t *| b
| 
| Now if you want to compute a given point along a Bezier spline, you can do
| 
| bezier :: Scalar -> [Vector2] -> Vector2
| bezier t ps = head $ last $ de_Casteljau t ps
| 
| You can chop one in half with
| 
| split :: Scalar -> [Vector2] -> ([Vector2], [Vector2])
| split t ps =
|   let pss = de_Casteljau t ps
|   in (map head pss, map last pss)
| 
| And any other beautiful incantations.
| 
| 
| Now, de Boor's algorithm is a generalisation of de Casteljau's 
| algorithm. It draws B-splines instead of Bezier-splines (since B-splines 
| generalise Bezier-splines). But I think I may have ACTUALLY MELTED MY 
| BRAIN attempting to comprehend it. Can anybody supply a straightforward 
| Haskell implementation?

It took me a while to get around to it, and another while to work it out, but here's what I've come up with.  First, here's a restatement of your code with a concrete choice of types (using Data.VectorSpace from the vector-space package) and a few minor stylistic changes just so things will line up with the generalized version better:

 > import Data.VectorSpace
 > 
 > interp a x y = lerp x y a
 > 
 > deCasteljau [] t = []
 > deCasteljau ps t = ps : deCasteljau (zipWith (interp t) ps (tail ps)) t
 > 
 > bezier ps = head . last . deCasteljau ps
 > 
 > split ps t = (map head pss, reverse (map last pss))
 >     where pss = deCasteljau ps t

To generalize to De Boor's algorithm, the primary change is the interpolation operation.  In De Casteljau's algorithm, every interpolation is over the same fixed interval 0 <= x <= 1.  For De Boor's algorithm we need a more general linear interpolation on the arbitrary interval [x0,x1], because all the interpolations in De Boor's recurrence are between pairs of knots, which have arbitrary values instead of just 0 or 1.

Because of Haskell's laziness, we can also take care of searching the result table for the correct set of control points at the same time, just by clamping the input to the desired interval and pre-emptively returning the corresponding 'y' if the input is outside that interval.  This way, to find the final interpolant we only need to go to the end of the table, as in 'deCasteljau'.  Unlike 'deCasteljau', only a portion of the table is actually computed (a triangular portion with the active control points as base and a path from the vertex of the triangle to the final entry in the table).

> interp x (x0,x1) (y0,y1)
>     |  x <  x0  = y0
>     |  x >= x1  = y1
>     | otherwise = lerp y0 y1 a
>     where
>         a = (x - x0) / (x1 - x0)

Computing the table is now nearly as straightforward as in De Casteljau's algorithm:

> deBoor p      _ [] x = []
> deBoor p (_:us) ds x = ds : deBoor (p-1) us ds' x
>     where
>         ds' = zipWith (interp x) (spans p us) (spans 1 ds)

Making use of a simple list function to select the spans:

> spans n xs = zip xs (drop n xs)

Note that the algorithm does not make use of @us!!0@ at all.  I believe this is correct, based both on the Wikipedia description of the algorithm and the implementations I've seen.  De Boor's recurrence seems to require an irrelevant choice of extra control point that would be, notionally, @ds!!(-1)@.  This control point has no actual influence inside the domain of the spline, although it /can/ affect values outside the domain (The domain being taken as the central portion of the knot vector where there are @p+1@ non-zero basis functions, forming a complete basis for the degree @p@ polynomials on that interval).

This implementation makes the arbitrary but justifiable choice that the extra control point be identical to the first, so that the position of the first knot is irrelevant. It could alternatively be written to take the extra control point as an argument or as a part of @ds@, in which case the caller would be required to supply an additional control point that does not actually influence the spline.

I initially found this result difficult to convince myself of even though it seems very plausible mathematically, because it seems to indicate that in general the position of the first and last knots are utterly irrelevant and I never saw any remarks to that effect in any of my reading on B-splines. Empirically, though, it seems to hold.  Moving an internal knot at one end of a basis function does not alter the shape of that function in the segment furthest opposite, which is basically the same effect the first knot should have on the first basis function (and the opposite segment is the only one that falls inside the domain of the spline).  It still may be that I'm wrong, and if anyone knows I'd love to hear about it, but I'm presently inclined to believe that this is correct.

Finally, the (very simple) driver to evaluate a B-spline:

> bspline p us ds = head . last . deBoor p us ds

And a nearly-as-simple driver to evaluate a NURBS curve (type signature included not because it couldn't be inferred but because it takes a bit of sorting out to understand, so I wanted to present it in a slightly more digestible form):

> nurbs :: (VectorSpace v, Scalar v ~ s,
>           VectorSpace s, Scalar s ~ Scalar v,
>           Fractional s, Ord s) =>
>      Int -> [s] -> [(v, s)] -> s -> v
> nurbs n us ds = project . bspline n us (map homogenize ds)
>     where
>         project (p,w) = recip w *^ p
>         homogenize (d,w) = (w *^ d, w)

For example, a NURBS circle (0 <= x <= 1):

> circle :: Double -> (Double, Double)
> circle = nurbs 2 us ds
>     where
>         us = [0,0,0,0.25,0.25,0.5,0.5,0.75,0.75,1,1,1]
>         ds = [ (( 1, 0),1)
>              , (( 1,-1),w)
>              , (( 0,-1),1)
>              , ((-1,-1),w)
>              , ((-1, 0),1)
>              , ((-1, 1),w)
>              , (( 0, 1),1)
>              , (( 1, 1),w)
>              , (( 1, 0),1)
>              ]
>         w = sqrt 0.5

You can also split your B-spline just like a Bezier curve:

> simpleSplit p us ds t = (map head dss, reverse (map last dss))
>     where dss = deBoor p us ds t

The resulting B-splines use the same knot vector as the original and, in general, will have many duplicated control points.  The ends of the splines can be cut off as long as you leave @p+1@ repeated control points, where @p@ is the degree of the spline.

Alternatively, you can do a bit more work in the split function and cut the knot vector at the split point and insert @p+1@ copies of the split point into the resulting knot vector:

> split p us ds t = ((us0, ds0), (us1, ds1))
>     where
>         us0 = takeWhile (<t) us ++ replicate (p+1) t
>         ds0 = trimTo (drop (p+1) us0) (map head dss)
>         
>         us1 = replicate (p+1) t ++ dropWhile (<=t) us
>         ds1 = reverse (trimTo (drop (p+1) us1) (map last dss))
>         
>         dss = deBoor p us ds t
>
>         trimTo list  xs = zipWith const xs list

    -- James


Misc. extras
=============

Effective basis splines for a given knot vector.  These are not the same as would be obtained by the Cox-De Boor recurrences ab initio, but rather the effective bases used by the de Boor algorithm as implemented above.  They are the same inside the domain of the spline, but outside they lose one polynomial degree with each knot span.

> basis :: Int -> [Double] -> Int -> Double -> Double
> basis p us i = bspline p us ds
>     where
>         m  = length us - p - 1
>         ds = take m (replicate i 0 ++ 1 : repeat 0)

A more robust version of 'deBoor' that deals with improper inputs and also is slightly more efficient in its handling of the knot vector spans:

 > import Data.List
 > import Data.VectorSpace

> deBoor' p _ _ _ | p < 0   = error "deBoor: negative degree is not allowed"
> deBoor' p us ds x = go us (padTo uHi zeroV ds)
>     where
>         -- Upper endpoints of the intervals are the same for
>         -- each row in the table (they just line up differently
>         -- with the lower endpoints):
>         uHi = drop (p+1) us
>         
>         -- On each pass, the lower endpoints of the 
>         -- interpolation intervals advance and the new 
>         -- coefficients are given by linear interpolation
>         -- on the current intervals, just as in 
>         -- De Casteljau's algorithm (this is the main
>         -- generalization: in a Bezier spline,
>         -- the interval is always [0,1], because
>         -- the knot vector is always 
>         -- @replicate (p+1) 0 ++ replicate (p+1) 1@).
>         go       _ [] = []
>         go (_:uLo) ds = ds : go uLo ds'
>             where
>                 ds' = zipWith4 (interp x) uLo uHi
>                                           ds (tail ds)
>         
>         -- Uses a slightly modified argument list for interp:
>         interp x x0 x1 y0 y1
>             |  x <  x0  = y0
>             |  x >= x1  = y1
>             | otherwise = lerp y0 y1 a
>             where
>                 a = (x - x0) / (x1 - x0)

And a couple more general utility functions used in deBoor':

> trimTo list  xs = zipWith const xs list
> padTo list z xs = trimTo list (xs ++ repeat z)

> deriv p us ds = zipWith (*^) (tail cs) (zipWith (^-^) (tail ds) ds)
>     where
>         cs = [fromIntegral p / (u0 - u1) | (u0, u1) <- spans p us]


Pastebin
========

NURBS circle:
us = [0,0,0,0.25,0.25,0.5,0.5,0.75,0.75,1,1,1];ds = [ (( 1, 0),1), (( 1,-1),w), (( 0,-1),1), ((-1,-1),w), ((-1, 0),1), ((-1, 1),w), (( 0, 1),1), (( 1, 1),w), (( 1, 0),1)] :: [((Double,Double),Double)];w = sqrt 0.5;n::Int; n=2

project (p,w) = recip w *^ p; homogenize (d,w) = (w *^ d, w)


let f = circle in sequence_ [printf "%g\t%g\n" x (f x) | x <- map (/1000) [0..1000]]

Basis functions:
let us = map (/10) [0..10] in sequence_ [printf "%g\t%g\t%g\t%g\t%g\t%g\t%g\n" x (y 0) (y 1) (y 2) (y 3) (y 4) (y 5) | x <- map (/1000) [0..1000], let y n = basis 4 us (n) x]

us = [0,0,0,0.45,0.45,0.5,0.5,0.75,0.75,1,1,1];ds = [ (( 1, 0),1), (( 1,-1),w), (( 0,-1),1), ((-1,-1),w), ((-1, 0),1), ((-1, 1),w), (( 0, 1),1), (( 1, 1),w), (( 1, 0),1)] :: [((Double,Double),Double)];w = sqrt 0.5;n::Int; n=2
