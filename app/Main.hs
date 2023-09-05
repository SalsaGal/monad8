{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad
import           Data.Array.Base
import           Data.Array.Storable
import           Data.ByteString       (ByteString, unpack)
import qualified Data.ByteString
import           Foreign
import           Foreign.C
import qualified Foreign.Marshal.Array as Marshal
import           GHC.Integer           (wordToInteger)
import           Linear
import qualified SDL
import           State
import           System.Environment

width, height :: Num a => a
width = 64
height = 32

chunks :: Int -> [a] -> [[a]]
chunks _ [] = []
chunks i xs = take i xs : chunks i (drop i xs)

byteStringToInts :: ByteString -> [Int]
byteStringToInts = map (\[a, b] -> a * 0x100 + b) . chunks 2 . map fromIntegral . unpack

withWindow :: (SDL.Window -> SDL.Renderer -> IO ()) -> IO ()
withWindow action = do
  SDL.initializeAll

  window <- SDL.createWindow "Monad-8" SDL.defaultWindow { SDL.windowInitialSize = V2 (width * 10) (height * 20) }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  action window renderer

  SDL.destroyRenderer renderer
  SDL.destroyWindow window

data TimingInfo = TimingInfo
  { time       :: Float
  , lastUpdate :: Float
  }

render :: Ptr CUInt -> IO ()
render buf = forM_ [0..height - 1] $ \y -> do
  forM_ [0..width - 1] $ \x -> do
    let index = width * y + x
    forM_ [0..4] (\i -> do
        let pixel = plusPtr buf $ index * 4 + i
        poke pixel $ CUInt 255
      )

upload :: SDL.Texture -> Int -> Int -> Ptr CUInt -> IO ()
upload texture width height source = do
  (lockedPtr, pitch) <- SDL.lockTexture texture Nothing
  let dest = castPtr lockedPtr :: Ptr CUInt
  Marshal.copyArray dest source (width * height)
  SDL.unlockTexture texture

updateTiming :: TimingInfo -> Float -> TimingInfo
updateTiming timingInfo newTime = timingInfo { lastUpdate = time timingInfo, time = newTime }

main :: IO ()
main = withWindow $ \window renderer -> do
  texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming (V2 width height)

  pixelBuffer <- Data.Array.Storable.newArray (0, width * height - 1) 0 :: IO (StorableArray Int CUInt)
  withStorableArray pixelBuffer render

  instructions <- getArgs >>= fmap unpack . Data.ByteString.readFile . Prelude.head

  let loop state timingInfo = do
        SDL.pollEvents

        withStorableArray pixelBuffer (upload texture width height)
        SDL.copy renderer texture Nothing Nothing

        SDL.present renderer

        newState <- update state

        keyState <- SDL.getKeyboardState
        newTimingInfo <- updateTiming timingInfo <$> SDL.time
        unless (keyState SDL.ScancodeEscape) (loop newState newTimingInfo)

  time <- SDL.time
  loop (newState instructions) $ TimingInfo time time

  SDL.destroyTexture texture
