module Interocitor where

type RootType = Interocitor -> Class Prog
type Prog = Action

struct Interocitor where
    time    :: Controllable Int
    buttons :: Array (Observable Bool)
    knobs   :: Array Knob
    
    nixies  :: Array Nixie
    speaker :: Speaker
    
    panic   :: Int -> Request ()

struct In a where
    get :: Request a

struct Out a where
    set :: a -> Action

struct Event a where
    onEvent :: (a -> Action) -> Request ()

struct Controllable a < In a, Out a
struct Observable a < In a, Event a

struct Nixie where
    number      :: Controllable Int
    intensity   :: Controllable Int

struct Knob where
    pressed     :: Observable Bool
    position    :: Observable Int

struct Speaker where
    power       :: Controllable Bool
    tone        :: Controllable Tone

struct Tone where
    volume  :: Int
    freq    :: Int
