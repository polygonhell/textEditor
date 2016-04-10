{-# LANGUAGE RecordWildCards #-}
module TTYRender where

import Data.Sequence as S
import Data.Foldable
import Data.List as L
import Control.Concurrent
import TextBuffer
import Text.Printf
import Control.Monad.Loops


drawLines :: Int -> Int -> BufferContent -> IO ()
drawLines _ _ b | b == S.empty = return ()
drawLines leftCol width b = do
  let h :< t = viewl b
      line = L.take width $ L.drop leftCol h
      str = printf "%s\n" line :: String
  putStr str
  drawLines leftCol width t

cls :: String
cls = printf "%c[2J" (toEnum 27 :: Char)

toPos :: Int -> Int -> String
toPos = printf "%c[%d;%dH" (toEnum 27 :: Char)

index :: String
index = printf "%cD" (toEnum 27 :: Char)

revIndex :: String
revIndex = printf "%cM" (toEnum 27 :: Char)

smoothScroll :: String
smoothScroll = printf "%c[?4h" (toEnum 27 :: Char)

setTopAndBottom :: Int -> Int -> String
setTopAndBottom = printf "%c[%d;%dr" (toEnum 27 :: Char) 

resetTopAndBottom :: String
resetTopAndBottom = printf "%c[r" (toEnum 27 :: Char) 

draw :: Int -> Int -> Int -> Int -> Buffer -> IO ()
draw topRow leftCol width height  b@Buffer{..} = do 

  let cls = printf "%c[2J" (toEnum 27 :: Char) ::String
      buffSlice = S.take height $ S.drop topRow content
  putStr cls
  -- putStr $ setTopAndBottom 0 5
  putStr resetTopAndBottom
  putStr $ toPos 0 0
  drawLines leftCol width buffSlice 

  -- putStr $ toPos 0 0
  -- putStr (concat $ L.replicate 3 revIndex )
  putStr $ toPos 2 5
  threadDelay $ 10 * 1000000




