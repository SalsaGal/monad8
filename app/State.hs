module State where

import           Data.Bits
import Data.Word
import Text.Printf

printHex :: PrintfArg a => String -> a -> IO ()
printHex "" = printf "0x%04x\n"
printHex label = printf $ label ++ ": 0x%04x\n"

data SystemState = SystemState
  { programCounter :: Int
  , v0             :: Int
  } deriving Show

defaultState :: SystemState
defaultState = SystemState
  { programCounter = 0
  , v0 = 0
  }

update :: [Word8] -> SystemState -> IO SystemState
update instructions state = do
  let pc = programCounter state
  printHex "PC" pc
  let opcode = fromIntegral (instructions !! pc) + 0x100 * fromIntegral (instructions !! (pc + 1))
  printHex "Opcode" opcode

  return $ case opcode .&. 0xF000 of
    0x1000 -> state { programCounter = opcode .&. 0x0FFF }
    _      -> state { programCounter = programCounter state + 1 }
