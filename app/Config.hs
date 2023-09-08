module Config where

import Foreign.C

data SystemConfig = SystemConfig
  { onColor  :: [CUInt]
  , offColor :: [CUInt]
  }
