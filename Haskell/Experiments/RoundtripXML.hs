{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
module Experiments.RoundtripXML where

import Control.Isomorphism.Partial.Iso
import Control.Category
import Data.PropertyList
import Text.Roundtrip
import Text.Roundtrip.Xml
import Prelude hiding (id, (.))

plArrayIso :: Show a => Iso [a] (PropertyListS a)
plArrayIso = unsafeMakeNamedIsoLR
    "array" (Just . PLArray) fromArray
    where
        fromArray (PLArray x) = Just x
        fromArray           _ = Nothing

plStringIso :: Show a => Iso String (PropertyListS a)
plStringIso = unsafeMakeNamedIsoLR
    "string" (Just . PLString) fromString
    where
        fromString (PLString s) = Just s
        fromString            _ = Nothing

plIntIso :: Show a => Iso Integer (PropertyListS a)
plIntIso = unsafeMakeNamedIsoLR
    "int" (Just . PLInt) fromInt
    where
        fromInt (PLInt x) = Just x
        fromInt         _ = Nothing

plRealIso :: Show a => Iso Double (PropertyListS a)
plRealIso = unsafeMakeNamedIsoLR
    "real" (Just . PLReal) fromReal
    where
        fromReal (PLReal x) = Just x
        fromReal          _ = Nothing

array       :: (Show a, XmlSyntax x) => x a -> x (PropertyListS a)
array item  = xmlElem "array" (plArrayIso <$> many item)
string      :: (Show a, XmlSyntax x) => x (PropertyListS a)
string      = xmlElem "string" (plStringIso . textStringIso <$> xmlText)
int         :: (Show a, XmlSyntax x) => x (PropertyListS a)
int         = xmlElem "int" (plIntIso . readShowTextIso <$> xmlText)
real        :: (Show a, XmlSyntax x) => x (PropertyListS a)
real        = xmlElem "real" (plRealIso . readShowTextIso <$> xmlText)

propertyListS :: (Show a, XmlSyntax x) => x a -> x (PropertyListS a)
propertyListS item = Prelude.foldl1 (<|>)
    [ array item
    , Experiments.RoundtripXML.string
    , int
    , real
    ]

-- using roundtrip-xml 0.2.0.1, this evaluates to:
-- Just "<int><real>3.141592653589793</real></array>"
-- Which is not only wrong but invalid XML.
nonsense = runXmlPrinterString (propertyListS (propertyListS empty)) (PLArray [PLReal pi])