{-# LANGUAGE TemplateHaskell, TypeSynonymInstances, FlexibleInstances,
        MultiParamTypeClasses, DeriveDataTypeable #-}
module Main where

import HOC
import Foundation (NSObject, NSObjectClass, alloc, init)
import Prelude hiding (init)

$(declareSelector "sayHi" [t| IO () |])

$(declareClass "Hello" "NSObject")
$(exportClass "Hello" "hello_" [
        InstanceMethod 'sayHi
    ])

hello_sayHi self = do
    putStrLn "hi!"

$(declareClass "Howdy" "Hello")
$(exportClass "Howdy" "howdy_" [
        InstanceMethod 'sayHi
        ])

howdy_sayHi self = do
    putStrLn "About to say hi:"
    super self # sayHi

$(declareClass "Antisocial" "Howdy")
$(exportClass "Antisocial" "" [])
instance Has_sayHi (Hello a)

main = do
    initializeClass_Hello
    initializeClass_Howdy
    initializeClass_Antisocial
    
    me <- _Antisocial # alloc >>= init
    me # sayHi