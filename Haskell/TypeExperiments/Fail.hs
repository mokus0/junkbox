module TypeExperiments.Fail where

import Control.Monad.RWS
import Control.Monad.Trans

-- A monad transformer that adds context reporting functionality to 'fail'
newtype FailT m a = FailT (RWST [(Maybe String, String)] () (Maybe String) m a)

instance Monad m => Monad (FailT m) where
    return = FailT . return
    fail msg = FailT $ do
        trace <- ask
        landmark <- get
        
        fail $ unwords $ filter (not.null)
            [ showWithLandmark landmark msg
            , showTrace trace
            ]
    FailT x >>= f = FailT (x >>= (\(FailT y) -> y).f)

showWithLandmark Nothing  x = show x
showWithLandmark (Just l) x = unwords [show x, "after", show l]

showTrace trace
    | null trace    = "at top level"
    | otherwise     = show
        [ showWithLandmark l section
        | (l, section) <- reverse trace
        ]

instance Monad f => Functor (FailT f) where
    fmap f (FailT x) = FailT (fmap f x)

instance MonadTrans FailT where
    lift x = FailT (lift x)

instance MonadIO m => MonadIO (FailT m) where
    liftIO x = FailT (liftIO x)

section s (FailT x) = FailT $ do
    saveLandmark <- get
    put Nothing
    res <- local ((saveLandmark, s):) x
    put saveLandmark
    return res

landmark s = FailT (put (Just s))

runFailT (FailT x) = do
    (res, _, _) <- runRWST x [] Nothing 
    return res