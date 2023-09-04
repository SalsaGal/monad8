{-# LANGUAGE OverloadedStrings #-}

module Main where
import           Linear
import qualified SDL

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

main :: IO ()
main = withWindow $ \window renderer -> do
  texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming (V2 width height)

  let loop = do
      SDL.pollEvents
      keystate <- SDL.getKeyboardState
      unless (keyState SDL.ScancodeEscape) loop

  SDL.destroyTexture texture
