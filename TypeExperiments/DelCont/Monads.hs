{-# LANGUAGE NoImplicitPrelude #-}
module TypeExperiments.DelCont.Monads where

import Prelude hiding (return, (>>=), (>>), fail, abs, filter)
import TypeExperiments.DelCont


--choice xs = shift' $ \k -> do
--    let go xs = case xs of
--            [] -> do
--                return ()
--            (x:xs) -> do
--                k (return x)
--                reset $ go xs
--    go xs

--choice xs = shift $ \k -> do
--    let go ys xs = case xs of
--            [] -> do
--                return (ys)
--            (x:xs) -> do
--                y <- k x
--                go (ys . y) xs
--    go id xs

filter p xs = shift $ \k -> do
    x <- xs
    if p x
        then k x
        else fail id

--collect xs = do
--    x <- xs

--collect xs = do
--    f <- reset $ do
--        x <- xs
--        return (x:)
--    return (f [])
--