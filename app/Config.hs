module Config where

import           Foreign.C

data SystemConfig = SystemConfig
  { onColor  :: [CUInt]
  , offColor :: [CUInt]
  }

defaultConfig :: SystemConfig
defaultConfig = SystemConfig {
    onColor = [255, 255, 255, 255],
    offColor = [0, 0, 0, 255]
  }
