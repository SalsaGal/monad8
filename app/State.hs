module State
( SystemState
, defaultState
) where

data SystemState = SystemState
  { programCounter :: Int
  , v0 :: Int
  }

defaultState :: SystemState
defaultState = undefined
