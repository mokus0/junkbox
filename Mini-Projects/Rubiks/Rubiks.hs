{-
 -	"Rubiks.hs"
 -	(c) 2008 James Cook
 -}

module Rubiks where

import Prelude hiding (Right, Left)
import qualified Prelude

import Data.Array

data Face = Top | Bottom | Left | Right | Front | Back
        deriving (Eq, Show, Ord, Enum, Bounded, Ix)

data Color = White | Yellow | Blue | Green | Orange | Red
        deriving (Eq, Show, Ord, Enum, Bounded, Ix)

color :: Face -> Color
color = toEnum . fromEnum

face :: Color -> Face
face = toEnum . fromEnum

type Edges = Array (Face, Face) (Color, Color)
type Corners = Array (Face, Face, Face) (Color, Color, Color)
type Cube = (Edges, Corners)

moves :: Face -> Int -> (Cube -> Cube)
moves f      0 cube = cube
moves Top    1 (es, cs) = undefined
moves Bottom 1 (es, cs) = undefined
moves Left   1 (es, cs) = undefined
moves Right  1 (es, cs) = undefined
moves Front  1 (es, cs) = undefined
moves Back   1 (es, cs) = undefined
moves f      n cube
        | (n >= 4) || (n < 0)   = moves f (n `mod` 4) cube
        | otherwise             = moves f 1 (moves f (n - 1) cube)