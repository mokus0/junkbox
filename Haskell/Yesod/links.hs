{-# LANGUAGE TypeFamilies, QuasiQuotes, MultiParamTypeClasses, TemplateHaskell, OverloadedStrings #-}
import Yesod

data Links = Links
    { nPages :: Int
    }

mkYesod "Links" [parseRoutes|
 / HomeR GET
 /page/#Int PageR GET
|]

instance Yesod Links where
    approot _ = ""

getHomeR  = defaultLayout [whamlet|<a href=@{PageR 1}>Go to page 1!|]
getPageR n = defaultLayout [whamlet|<a href=@{PageR next}>Go to page #{next}!|]
    where next = n + 1

main = warpDebug 3000 (Links 10)
