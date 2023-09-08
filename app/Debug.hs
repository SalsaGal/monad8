module Debug where
import           Text.Printf

data DebugOptions = DebugOptions
  { printOpcode  :: Bool
  , printAddress :: Bool
  }

printHex :: PrintfArg a => String -> a -> IO ()
printHex ""    = printf "0x%04x\n"
printHex label = printf $ label ++ ": 0x%04x\n"
