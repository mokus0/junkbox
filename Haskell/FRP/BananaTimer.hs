{-# LANGUAGE RecordWildCards, RankNTypes #-}
module FRP.BananaTimer where

import Data.Time
import Reactive.Banana

data ClockControls t = ClockControls
    { initialTime       :: Double
    , unitsPerSecond    :: Discrete t Double
    , referenceClock    :: Maybe (Clock t)
    }

defaultClockControls = ClockControls
    { initialTime       = 0
    , unitsPerSecond    = pure 1
    , referenceClock    = Nothing
    }

data Clock t = Clock
    { settings          :: ClockControls t
    , currentTime       :: Behavior t Double
    }

systemClock :: UTCTime -> NetworkDescription t (Clock t)
systemClock epoch = do
    let getTime = currentSecondsSince epoch
    currentTime <- fromPoll getTime
    startTime   <- liftIO   getTime
    
    return Clock
        { settings = defaultClockControls
            { initialTime = startTime }
        , ..
        }

currentSecondsSince :: UTCTime -> IO Double
currentSecondsSince epoch = do
    now <- getCurrentTime
    let d = diffUTCTime now epoch
    return $! realToFrac d

data ClockState = ClockState
    { rate      :: !Double  -- ^ the ratio of slave's clock rate to master's clock rate
    , refTime   :: !Double  -- ^ the master clock's time at which the multiplier last changed
    , simTime   :: !Double  -- ^ the slave clock's time at which the multiplier last changed
    } deriving (Eq, Ord, Show)

mkClock :: ClockControls t -> NetworkDescription t (Clock t)
mkClock simClockSettings = do
    refClock <- maybe (liftIO getCurrentTime >>= systemClock) return (referenceClock simClockSettings)
    
    let refClockTime = currentTime refClock
        refClockSettings = settings refClock
        
        multiplier = liftA2 (/) (unitsPerSecond simClockSettings) (unitsPerSecond refClockSettings)
        
        initialState = ClockState (initial multiplier) (initialTime refClockSettings) (initialTime simClockSettings)
        instantState = ClockState <$> value multiplier <*> refClockTime <*> simClockTime
        -- snapshot of state at last multiplier change event
        clockState = stepper initialState (instantState <@ changes multiplier)
        
        dt = liftA2 (-) refClockTime (refTime <$> clockState)
        simClockTime = liftA2 (+) (liftA2 (*) dt (value multiplier)) (simTime <$> clockState)
    
    return Clock
        { settings      = simClockSettings { referenceClock = Just refClock }
        , currentTime   = simClockTime
        }

testClock trigger clk = do
    reactimate (dump "time" <$> currentTime clk <@ trigger)
    reactimate (dump "rate" <$> changes (unitsPerSecond (settings clk)))
    where
        dump label t = putStrLn $ concat [label, ": ", show t]

testSysClk trigger = do
    epoch <- liftIO getCurrentTime
    testClock trigger =<< systemClock epoch

testSimpleClk trigger = do
    (rateChangesH, changeRate) <- liftIO newAddHandler
    let controlRate "+" = changeRate (+ 0.1)
        controlRate "-" = changeRate (subtract 0.1)
        controlRate  r  = case reads r of
            [(x, "")]   -> changeRate (const x)
            _           -> return ()
    
    rateChanges <- fromAddHandler rateChangesH
    let rate = accumD 0 rateChanges
    
    testClock trigger =<< mkClock defaultClockControls {unitsPerSecond = rate}
    reactimate (controlRate <$> trigger)

withCon :: (forall t. Event t String -> NetworkDescription t ()) -> IO ()
withCon testNetwork = do
    (conH, onCon) <- newAddHandler
    compile (fromAddHandler conH >>= testNetwork) >>= actuate
    let suck = getLine >>= \x -> case x of
            "q"     -> return ()
            line    -> onCon line >> suck
    
    suck

main = do
    putStrLn $ unlines
        [ "simple toy clock"
        , "================"
        , "starts at 0.0 sec/sec and t = 0.0"
        , "+/-/<number> to change rate"
        , "q to exit"
        , "anything but q to print current time"
        ]
    withCon testSimpleClk