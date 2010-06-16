> {-# LANGUAGE MultiParamTypeClasses #-}
> {-# OPTIONS_GHC -O #-}
> module Panics.RedundantSpecialize where
> 
> class Foo a where
>     foo :: a -> a
>     foo = id
>     
> instance Foo Double where
>     {-# SPECIALIZE instance Foo Double #-}

Compiling this module:

$ ghc-6.13.20100616 Panics/RedundantSpecialize.lhs 
[1 of 1] Compiling Panics.RedundantSpecialize ( Panics/RedundantSpecialize.lhs, Panics/RedundantSpecialize.o )
ghc: panic! (the 'impossible' happened)
  (GHC version 6.13.20100616 for i386-apple-darwin):
	Template variable unbound in rewrite rule
    $fFooDouble{v Xf} [lid]
    [$fFooDouble{v Xf} [lid]]
    [$fFooDouble{v Xf} [lid]]
    []
    []

Please report this as a GHC bug:  http://www.haskell.org/ghc/reportabug

