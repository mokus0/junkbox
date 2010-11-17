{-# LANGUAGE DoRec #-}
module Functions.New where

import Control.Monad.Fix
import Data.IORef

type New t = t -> t
new :: New t -> t
new init = self where self = init self

ones = new (1:)

data Animal = Animal {
    catchPhrase :: String,
    speak       :: IO ()
}

protoAnimal :: New Animal
protoAnimal self = Animal {
    speak = putStrLn (catchPhrase self)
}

cat :: New Animal
cat self = (protoAnimal self) { catchPhrase = "Meow" }

dog :: New Animal
dog self = (protoAnimal self) { catchPhrase = "Woof", speak = putStrLn "Rowwrlrw" }


type NewM m t = t -> m t

newM :: MonadFix m => NewM m t -> m t
newM init = do
    rec self <- init self
    return self

data AnimalM m = AnimalM {
    catchPhraseM    :: m String,
    speakM          :: m ()
}

protoAnimalIO :: NewM IO (AnimalM IO)
protoAnimalIO self = do
    putStrLn "making protoAnimal"
    return AnimalM
        { speakM = catchPhraseM self >>= putStrLn
        }

catIO :: NewM IO (AnimalM IO)
catIO self = do
    self <- protoAnimalIO self
    meowCount <- newIORef 1
    
    let meow = do
            n <- readIORef meowCount
            writeIORef meowCount $! (n+1)
            if n `mod` 7 /= 0 
                then return "Meow"
                else return "Meeeeeowow!"
    
    return self {catchPhraseM = meow}


dogIO :: NewM IO (AnimalM IO)
dogIO self = do
    self <- protoAnimalIO self
    return self
        { catchPhraseM = return "Woof"
        , speakM = putStrLn "Rowwrlrw"
        }
