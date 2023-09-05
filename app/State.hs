module State where

import           Data.Bits
import           Data.Word
import           Text.Printf

printHex :: PrintfArg a => String -> a -> IO ()
printHex ""    = printf "0x%04x\n"
printHex label = printf $ label ++ ": 0x%04x\n"

data SystemState = SystemState
  { memory         :: [Word8]
  , screen         :: [[Bool]]
  , programCounter :: Int
  , v0             :: Int
  } deriving Show

newState :: [Word8] -> SystemState
newState memory = SystemState
  { memory = replicate 0x200 0 ++ memory
  , screen = replicate 32 $ replicate 64 False
  , programCounter = 0x200
  , v0 = 0
  }

update :: SystemState -> IO SystemState
update state = do
  let instructions = memory state
  let pc = programCounter state
  printHex "PC" pc
  let opcode = 0x100 * fromIntegral (instructions !! pc) + fromIntegral (instructions !! (pc + 1))
  printHex "Opcode" opcode

  return $ case opcode .&. 0xF000 of
    0x1000 -> state { programCounter = opcode .&. 0x0FFF }
    _      -> state { programCounter = programCounter state + 2 }
