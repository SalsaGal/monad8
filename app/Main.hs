{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Linear
import qualified SDL
import Control.Monad

width, height :: Num a => a
width = 64
height = 32

withWindow :: (SDL.Window -> SDL.Renderer -> IO ()) -> IO ()
withWindow action = do
  SDL.initializeAll

  window <- SDL.createWindow "Monad-8" SDL.defaultWindow { SDL.windowInitialSize = V2 (width * 10) (height * 20) }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  action window renderer

  SDL.destroyRenderer renderer
  SDL.destroyWindow window

data TimingInfo = TimingInfo
  { time :: Float
  , lastUpdate :: Float
  }

updateTiming :: TimingInfo -> Float -> TimingInfo
updateTiming timingInfo newTime = timingInfo { lastUpdate = time timingInfo, time = newTime }

main :: IO ()
main = withWindow $ \window renderer -> do
  texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming (V2 width height)

  let loop timingInfo = do
        SDL.pollEvents

        SDL.present renderer

        keyState <- SDL.getKeyboardState
        newTimingInfo <- updateTiming timingInfo <$> SDL.time
        unless (keyState SDL.ScancodeEscape) (loop newTimingInfo)

  time <- SDL.time
  loop $ TimingInfo time time

  SDL.destroyTexture texture
