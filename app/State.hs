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
  , vRegisters     :: [Word8]
  } deriving Show

blankScreen :: [[Bool]]
blankScreen = replicate 32 $ replicate 64 False

incrementPC :: SystemState -> SystemState
incrementPC state = state { programCounter = programCounter state + 2 }

newState :: [Word8] -> SystemState
newState memory = SystemState
  { memory = replicate 0x200 0 ++ memory
  , screen = blankScreen
  , programCounter = 0x200
  , vRegisters = replicate 0xf 0
  }

update :: SystemState -> IO SystemState
update state = do
  let instructions = memory state
  let pc = programCounter state
  printHex "PC" pc
  let opcode = 0x100 * fromIntegral (instructions !! pc) + fromIntegral (instructions !! (pc + 1))
  printHex "Opcode" opcode

  return $ case opcode of
    0x00e0 -> incrementPC state { screen = blankScreen }
    _ -> case opcode .&. 0xF000 of
      0x1000 -> state { programCounter = opcode .&. 0x0FFF }
      0x6000 -> do
        let regs = vRegisters state
        let register = opcode .&. 0x0F00 `div` 0x0F00
        let value = opcode .&. 0x00FF
        incrementPC state { vRegisters = take register regs ++ [fromIntegral value] ++ drop register regs }
      _      -> incrementPC state
