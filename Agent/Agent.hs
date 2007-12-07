#!runhaskell
{-
 -	"/Users/mokus/Desktop/Haskell/Agent/Agent.hs"
 -	(c) 2007 James Cook
 -}

module Agent where

import Control.Concurrent.STM

data Agent st obs cmd = Agent {
		state :: TVar st,
		brain :: TVar (Agent st obs cmd -> TChan obs -> TChan cmd -> IO ())
	} deriving (Eq)

mkAgent st br = do
	st' <- newTVar st
	br' <- newTVar br
	return (Agent st' br')

mkSimplerAgent ::	st 
				 -> (st -> [obs] -> (st, [cmd]))
				 -> STM (Agent st obs cmd)

mkSimplerAgent st br = mkAgent st (wrapBrain br)
	where
		(wrapBrain br) self obsChan cmdChan = do
			st <- atomically (readTVar (state self))
			obs <- mRead obsChan
			
			let (newSt, cmd) = br st obs 
			
			mWrite cmdChan cmd
			atomically (writeTVar (state self) newSt)
		mRead chan = do
			datum <- atomically (readNow chan)
			case datum of
				Nothing -> return []
				Just x -> do
					rest <- mRead chan
					return (x:rest)
		readNow chan = do
			isEmpty <- isEmptyTChan chan
			case isEmpty of
				True -> return Nothing
				False -> do
					x <- readTChan chan
					return (Just x)
		mWrite chan stuff = mapM_ (atomically . (writeTChan chan)) stuff
