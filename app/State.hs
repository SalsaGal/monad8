module State where

import           Control.Monad
import           Data.Bits
import           Data.Word
import           Debug

data SystemState = SystemState
  { memory         :: [Word8]
  , screen         :: [[Bool]]
  , programCounter :: Word16
  , vRegisters     :: [Word8]
  , indexRegister  :: Word16
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
  let opcode = 0x100 * fromIntegral (instructions !! fromIntegral pc) + fromIntegral (instructions !! (fromIntegral pc + 1)) :: Word16
  when (printOpcode debug) $ printHex "Opcode" opcode

  return $ case opcode of
    0x00e0 -> incrementPC state { screen = blankScreen }
    _ -> case opcode .&. 0xf000 of
      0x1000 -> state { programCounter = opcode .&. 0x0fff }
      0x6000 -> do
        let regs = vRegisters state
        let register = fromIntegral opcode .&. 0x0f00 `div` 0x0100
        let value = fromIntegral opcode .&. 0x00ff :: Word8
        incrementPC state { vRegisters = take register regs ++ [fromIntegral value] ++ drop (register + 1) regs }
      0xa000 -> incrementPC state { indexRegister = fromIntegral opcode .&. 0x0fff }
      0xd000 -> incrementPC state { screen = do
            let spriteX = vRegisters state !! (fromIntegral opcode .&. 0x0f00 `div` 0x0100)
            let spriteY = vRegisters state !! (fromIntegral opcode .&. 0x00f0 `div` 0x0010)
            let height = opcode .&. 0x000f
            zipWith (\y row -> zipWith (\x pixel ->
                if x >= spriteX && x < spriteX + 8 && y >= spriteY && y < spriteY + fromIntegral height
                  then do
                    let byte = fromIntegral $ y - spriteY :: Int
                    let bit = fromIntegral $ x - spriteX  :: Int
                    0 /= ((128 `shiftR` bit) .&. instructions !! (fromIntegral (indexRegister state) + byte))
                  else screen state !! fromIntegral y !! fromIntegral x
              ) [0 :: Word8 ..] row) [0 :: Word8 ..] (screen state)
          }
      _      -> incrementPC state
