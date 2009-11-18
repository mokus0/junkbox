{-
 -      ``stringPatTest''
 -      (c) 2008 James Cook
 -}

module StringPatTest where

f "aoeu" = True
f "asdf" = True
f _ = False

g ('a':'o':'e':'u':[]) = True
g ('a':'s':'d':'f':[]) = True
g _ = False

