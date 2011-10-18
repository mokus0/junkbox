{-# LANGUAGE ConstraintKinds, UndecidableInstances, ScopedTypeVariables,
        FlexibleInstances #-}
module ConstraintKinds.Typeable where

import ConstraintKinds.Dict
import Control.Applicative
import Data.Typeable
import Data.Tagged

class TypeableCxt c where
    typeOfCxt :: Tagged (Dict c) TypeRep

class TypeableCxt1 p where
    typeOfCxt1 :: Tagged (Dict (p t)) TypeRep

class TypeableCxt2 p where
    typeOfCxt2 :: Tagged (Dict (p t1 t2)) TypeRep

instance (TypeableCxt1 p, Typeable t) => TypeableCxt (p t) where
    typeOfCxt = do
        pCon <- typeOfCxt1
        return (mkAppTy pCon (typeOf (undefined :: t)))

instance (TypeableCxt2 p, Typeable t) => TypeableCxt1 (p t) where
    typeOfCxt1 = do
        pCon <- typeOfCxt2
        return (mkAppTy pCon (typeOf (undefined :: t)))

instance TypeableCxt2 (~) where
    typeOfCxt2 = Tagged (mkTyConApp eqCon [])
        where
            eqCon = mkTyCon3 "base" "Prelude" "(~)"



instance TypeableCxt1 Eq where
    typeOfCxt1 = Tagged (mkTyConApp con [])
        where
            con = mkTyCon3 "base" "Prelude" "Eq"

instance TypeableCxt1 Ord where
    typeOfCxt1 = Tagged (mkTyConApp con [])
        where
            con = mkTyCon3 "base" "Prelude" "Ord"

instance TypeableCxt1 Show where
    typeOfCxt1 = Tagged (mkTyConApp con [])
        where
            con = mkTyCon3 "base" "Prelude" "Show"

instance TypeableCxt1 Read where
    typeOfCxt1 = Tagged (mkTyConApp con [])
        where
            con = mkTyCon3 "base" "Prelude" "Read"

instance TypeableCxt1 Enum where
    typeOfCxt1 = Tagged (mkTyConApp con [])
        where
            con = mkTyCon3 "base" "Prelude" "Enum"

instance TypeableCxt1 Bounded where
    typeOfCxt1 = Tagged (mkTyConApp con [])
        where
            con = mkTyCon3 "base" "Prelude" "Bounded"

instance TypeableCxt1 Num where
    typeOfCxt1 = Tagged (mkTyConApp con [])
        where
            con = mkTyCon3 "base" "Prelude" "Num"

instance TypeableCxt1 Fractional where
    typeOfCxt1 = Tagged (mkTyConApp con [])
        where
            con = mkTyCon3 "base" "Prelude" "Fractional"

instance TypeableCxt1 Real where
    typeOfCxt1 = Tagged (mkTyConApp con [])
        where
            con = mkTyCon3 "base" "Prelude" "Real"

instance TypeableCxt1 Integral where
    typeOfCxt1 = Tagged (mkTyConApp con [])
        where
            con = mkTyCon3 "base" "Prelude" "Integral"

instance TypeableCxt1 Floating where
    typeOfCxt1 = Tagged (mkTyConApp con [])
        where
            con = mkTyCon3 "base" "Prelude" "Floating"

instance TypeableCxt1 RealFrac where
    typeOfCxt1 = Tagged (mkTyConApp con [])
        where
            con = mkTyCon3 "base" "Prelude" "RealFrac"

instance TypeableCxt1 RealFloat where
    typeOfCxt1 = Tagged (mkTyConApp con [])
        where
            con = mkTyCon3 "base" "Prelude" "RealFloat"

instance Typeable1 f => TypeableCxt (Functor f) where
    typeOfCxt = Tagged (mkTyConApp con [typeOf1 (undefined :: f a)])
        where
            con = mkTyCon3 "base" "Prelude" "Functor"

instance Typeable1 f => TypeableCxt (Monad f) where
    typeOfCxt = Tagged (mkTyConApp con [typeOf1 (undefined :: f a)])
        where
            con = mkTyCon3 "base" "Prelude" "Monad"

instance TypeableCxt c => Typeable (Dict c) where
    typeOf it = mkTyConApp dictCon [untag (typeOfCxt :: Tagged (Dict c) TypeRep)]
        where
            dictCon = mkTyCon3 "junkbox" "ConstraintKinds.Dict" "Dict"

instance TypeableCxt1 Typeable where
    typeOfCxt1 = Tagged (mkTyConApp con [])
        where
            con = mkTyCon3 "base" "Data.Typeable" "Typeable"

instance Typeable1 t => TypeableCxt (Typeable1 t) where
    typeOfCxt = Tagged (mkTyConApp con [typeOf1 (undefined :: t a)])
        where
            con = mkTyCon3 "base" "Data.Typeable" "Typeable1"

instance Typeable2 t => TypeableCxt (Typeable2 t) where
    typeOfCxt = Tagged (mkTyConApp con [typeOf2 (undefined :: t a b)])
        where
            con = mkTyCon3 "base" "Data.Typeable" "Typeable2"

instance Typeable3 t => TypeableCxt (Typeable3 t) where
    typeOfCxt = Tagged (mkTyConApp con [typeOf3 (undefined :: t a b c)])
        where
            con = mkTyCon3 "base" "Data.Typeable" "Typeable3"

instance Typeable4 t => TypeableCxt (Typeable4 t) where
    typeOfCxt = Tagged (mkTyConApp con [typeOf4 (undefined :: t a b c d)])
        where
            con = mkTyCon3 "base" "Data.Typeable" "Typeable4"

instance Typeable5 t => TypeableCxt (Typeable5 t) where
    typeOfCxt = Tagged (mkTyConApp con [typeOf5 (undefined :: t a b c d e)])
        where
            con = mkTyCon3 "base" "Data.Typeable" "Typeable5"

instance Typeable6 t => TypeableCxt (Typeable6 t) where
    typeOfCxt = Tagged (mkTyConApp con [typeOf6 (undefined :: t a b c d e f)])
        where
            con = mkTyCon3 "base" "Data.Typeable" "Typeable6"

instance Typeable7 t => TypeableCxt (Typeable7 t) where
    typeOfCxt = Tagged (mkTyConApp con [typeOf7 (undefined :: t a b c d e f g)])
        where
            con = mkTyCon3 "base" "Data.Typeable" "Typeable7"

instance TypeableCxt () where
    typeOfCxt = Tagged (mkTyConApp con [])
        where con = mkTyCon3 "base" "Prelude" "()"

instance (TypeableCxt a, TypeableCxt b) => TypeableCxt (a, b) where
    typeOfCxt = Tagged (mkTyConApp con 
            [ untag (typeOfCxt :: Tagged (Dict a) TypeRep)
            , untag (typeOfCxt :: Tagged (Dict b) TypeRep)
            ])
        where con = mkTyCon3 "base" "Prelude" "(,)"

instance (TypeableCxt a, TypeableCxt b, TypeableCxt c) => TypeableCxt (a, b, c) where
    typeOfCxt = Tagged (mkTyConApp con 
            [ untag (typeOfCxt :: Tagged (Dict a) TypeRep)
            , untag (typeOfCxt :: Tagged (Dict b) TypeRep)
            , untag (typeOfCxt :: Tagged (Dict c) TypeRep)
            ])
        where con = mkTyCon3 "base" "Prelude" "(,,)"

-- and so on...

