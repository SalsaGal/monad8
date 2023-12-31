{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Config
import           Control.Monad
import           Data.Array.Base
import           Data.Array.Storable
import           Data.ByteString       (ByteString, unpack)
import qualified Data.ByteString
import           Debug
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

  window <- SDL.createWindow "Monad-8" SDL.defaultWindow { SDL.windowInitialSize = V2 (width * 10) (height * 10) }
  renderer <- SDL.createRenderer window (-1) SDL.defaultRenderer

  action window renderer

  SDL.destroyRenderer renderer
  SDL.destroyWindow window

data TimingInfo = TimingInfo
  { time       :: Float
  , lastUpdate :: Float
  }

render :: SystemConfig -> [[Bool]] -> Ptr CUInt -> IO ()
render config screen buf = forM_ [0..height - 1] $ \y -> do
  forM_ [0..width - 1] $ \x -> do
    let index = width * y + x
    -- For some reason these seem to be ABGR? TODO Work out why
    let colors = reverse $ zip (onColor config) (offColor config)
    forM_ (zip [0..3] colors) (\(i, (on, off)) -> do
        let pixel = plusPtr buf $ index * 4 + i
        let color = screen !! y !! x
        poke pixel $ if color then on else off
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
  instructions <- getArgs >>= fmap unpack . Data.ByteString.readFile . Prelude.head

  texture <- SDL.createTexture renderer SDL.RGBA8888 SDL.TextureAccessStreaming (V2 width height)

  let state = newState instructions
  let debugOptions = DebugOptions {
      printOpcode = False,
      printAddress = False
    }

  pixelBuffer <- Data.Array.Storable.newArray (0, width * height - 1) 0 :: IO (StorableArray Int CUInt)

  let loop state timingInfo = do
        SDL.pollEvents

        withStorableArray pixelBuffer $ render defaultConfig $ screen state
        withStorableArray pixelBuffer (upload texture width height)
        SDL.copy renderer texture Nothing Nothing

        SDL.present renderer

        newState <- update debugOptions state

        keyState <- SDL.getKeyboardState
        newTimingInfo <- updateTiming timingInfo <$> SDL.time
        unless (keyState SDL.ScancodeEscape) (loop newState newTimingInfo)

  time <- SDL.time
  loop state $ TimingInfo time time

  SDL.destroyTexture texture
