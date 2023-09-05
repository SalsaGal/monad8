module State where

import           Data.Bits
import           Data.Word
import Debug
import Control.Monad

data SystemState = SystemState
  { memory         :: [Word8]
  , screen         :: [[Bool]]
  , programCounter :: Int
  , vRegisters     :: [Word8]
  , indexRegister  :: Word16      -- Meant to be 12 bits
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
  , indexRegister = 0
  }

update :: DebugOptions ->  SystemState -> IO SystemState
update debug state = do
  let instructions = memory state
  let pc = programCounter state
  when (printAddress debug) $ printHex "PC" pc
  let opcode = 0x100 * fromIntegral (instructions !! pc) + fromIntegral (instructions !! (pc + 1))
  when (printOpcode debug) $ printHex "Opcode" opcode

  return $ case opcode of
    0x00e0 -> incrementPC state { screen = blankScreen }
    _ -> case opcode .&. 0xf000 of
      0x1000 -> state { programCounter = opcode .&. 0x0fff }
      0x6000 -> do
        let regs = vRegisters state
        let register = opcode .&. 0x0f00 `div` 0x0f00
        let value = opcode .&. 0x00ff
        incrementPC state { vRegisters = take register regs ++ [fromIntegral value] ++ drop register regs }
      0xa000 -> incrementPC state { indexRegister = fromIntegral opcode .&. 0x0fff }
      _      -> incrementPC state
