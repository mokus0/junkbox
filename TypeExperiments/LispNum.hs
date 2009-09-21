#!runhaskell
{-# OPTIONS -fglasgow-exts -fallow-undecidable-instances -fextended-default-rules #-}
{-
 -	"LispNum.hs"
 -	(c) 2007 James Cook
 -}

-- This is a rather pointless exercise.  It's a pain to use, because
-- the types of numeric constants don't unify automatically.

module TypeExperiments.LispNum where

import Prelude
	hiding ((+))
import qualified Prelude

-- functional dependencies help a bit.  extended default rules don't seem to.
class LispNum a b | a -> b
	where
		(+) :: a -> b

--instance Num a => LispNum a a
--	where
--		(+) a = a

instance (Prelude.Num a) => LispNum (a,a) a
	where
		(+) (a,b) = (Prelude.+) a b

instance (LispNum (a,c) d, LispNum (b,e) c) => LispNum (a, (b,e)) d
	where
		(+) (a, (b,e)) = (+) (a, ((+) (b,e)) :: c)

