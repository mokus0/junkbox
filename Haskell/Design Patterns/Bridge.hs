{-
 -      ``Bridge''
 -      (c) 2008 Cook, J. MR  SSD, Inc.
 -      
 -      nearly a straight translation of the java example
 -      on wikipedia as of 23 July 2008.
 -      
 -      Changes:
 -      - "resizeByPercentage" actually uses its argument as a percentage
 -      - made existential type in CircleShape explicit
 -}
{-# LANGUAGE
        ExistentialQuantification
  #-}

module Bridge where

import Text.Printf

{- "Implementor" -}
class DrawingAPI api where
        {- drawCircle api (x,y) radius -}
        drawCircle :: api -> (Double, Double) -> Double -> IO ()

{- Unspecified ("Existential") Implementor -}
data DrawingAPIE = forall api. DrawingAPI api => DrawingAPIE api
instance DrawingAPI DrawingAPIE where
        drawCircle (DrawingAPIE api) = drawCircle api

{- "Concrete Implementor 1/2" -}
data DrawingAPI1 = DrawingAPI1
instance DrawingAPI DrawingAPI1 where
        drawCircle DrawingAPI1 pt radius = do
                printf "API 1: circle at %s, radius %g\n" (show pt) radius

{- "Concrete Implementor 2/2" -}
data DrawingAPI2 = DrawingAPI2
instance DrawingAPI DrawingAPI2 where
        drawCircle DrawingAPI2 pt radius = do
                printf "API 2: circle at %s, radius %g\n" (show pt) radius

{- "Abstraction" -}
class Shape s where
        draw :: s -> IO ()
        resizeByPercentage :: Double -> s -> s

{- "Refined Abstraction" -}
data CircleShape
        = CircleShape { circleCentre            :: (Double, Double)
                      , circleRadius            :: Double
                      , circleDrawingAPI        :: DrawingAPIE
                      }
instance Shape CircleShape where
        draw circle = drawCircle (circleDrawingAPI circle)
                                 (circleCentre     circle)
                                 (circleRadius     circle)
        resizeByPercentage p circle =
                circle { circleRadius = circleRadius circle * p / 100 }

{- "Client" -}
main = do
        let newCircle pt rad api = CircleShape pt rad (DrawingAPIE api)
        let shapes = [ newCircle (1,2) 3  DrawingAPI1
                     , newCircle (5,7) 11 DrawingAPI2
                     ]
        
        mapM_ (draw . resizeByPercentage 250) shapes
