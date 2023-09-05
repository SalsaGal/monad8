module State where

import           Data.Bits
import Data.Word

data SystemState = SystemState
  { programCounter :: Int
  , v0             :: Int
  } deriving Show

defaultState :: SystemState
defaultState = SystemState
  { programCounter = 0
  , v0 = 0
  }

update :: [Word8] -> SystemState -> SystemState
update instructions state = do
  let pc = programCounter state
  let opcode = fromIntegral (instructions !! pc) + 0x100 * fromIntegral (instructions !! (pc + 1))

  case opcode .&. 0xF000 of
    0x1000 -> state { programCounter = opcode .&. 0x0FFF }
    _      -> state { programCounter = programCounter state + 1 }
