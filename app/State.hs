module State where

import           Data.Bits

data SystemState = SystemState
  { programCounter :: Int
  , v0             :: Int
  } deriving Show

defaultState :: SystemState
defaultState = SystemState
  { programCounter = 0
  , v0 = 0
  }

update :: Int -> SystemState -> SystemState
update opcode state = case opcode .&. 0xF000 of
    0x1000 -> state { programCounter = opcode .&. 0x0FFF }
    _      -> state { programCounter = programCounter state + 1 }
