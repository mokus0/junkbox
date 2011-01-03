{-# LANGUAGE ViewPatterns #-}

-- let rec (|Expr|) (P(f, xs)) = Expr(loop (' ', f, xs))
-- and loop = function
--   | ' ' as oop, f, ('+' | '-' as op)::P(g, xs)
--   | (' ' | '+' | '-' as oop), f, ('*' | '/' as op)::P(g, xs) ->
--       let h, xs = loop (op, g, xs)
--       let op = match op with
--         | '+' -> (+) | '-' -> (-) | '*' -> (*) | '/' -> (/)
--       loop (oop, op f h, xs)
--   | _, f, xs -> f, xs
-- and (|P|) = function
--   | '('::Expr(f, ')'::xs) -> P(f, xs)
--   | c::xs when '0' <= c && c <= '9' -> P(int(string c), xs)

---- Initial translation

expr (p -> (f, xs)) = loop ' ' f xs

loop oop@' ' f (op@'+' : (p -> (g, xs))) = body oop f op g xs
loop oop@' ' f (op@'-' : (p -> (g, xs))) = body oop f op g xs

loop oop@' ' f (op@'*' : (p -> (g, xs))) = body oop f op g xs
loop oop@' ' f (op@'/' : (p -> (g, xs))) = body oop f op g xs
loop oop@'+' f (op@'*' : (p -> (g, xs))) = body oop f op g xs
loop oop@'+' f (op@'/' : (p -> (g, xs))) = body oop f op g xs
loop oop@'-' f (op@'*' : (p -> (g, xs))) = body oop f op g xs
loop oop@'-' f (op@'/' : (p -> (g, xs))) = body oop f op g xs

loop _ f xs = (f, xs)

body oop f op g xs = loop oop (op' f h) xs'
    where
        (h, xs') = loop op g xs
        op' = case op of
            '+' -> (+); '-' -> (-); '*' -> (*); '/' -> (/)

p ('(' : (expr -> (f, ')' : xs))) = (f, xs)
p (c   : xs)
    | '0' <= c && c <= '9'      = (read [c], xs)


---- Exploring options for simplification:

-- expr (p -> (f, xs)) = loop ' ' f xs
-- 
-- loop (oneOf " "   -> Just oop) f ((oneOf "+-" -> Just op) : (p -> (g,xs))) = body oop f op g xs
-- loop (oneOf " +-" -> Just oop) f ((oneOf "*/" -> Just op) : (p -> (g,xs))) = body oop f op g xs
-- loop _ f xs = (f, xs)
-- 
-- body oop f op g xs = loop oop (op' f h) xs'
--     where
--         (h, xs') = loop op g xs
--         op' = case op of
--             '+' -> (+); '-' -> (-); '*' -> (*); '/' -> (/)
-- 
-- p ('(' : (expr -> (f, ')' : xs))) = (f, xs)
-- p (c   : xs)
--     | '0' <= c && c <= '9'      = (read [c], xs)
-- 
-- oneOf xs x
--     | x `elem` xs   = Just x
--     | otherwise     = Nothing

