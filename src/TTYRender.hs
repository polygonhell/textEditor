{-# LANGUAGE RecordWildCards #-}
module TTYRender where

import Data.Sequence as S
import Data.Foldable
import Data.List as L
import TextBuffer
import Text.Printf


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

scrollUp :: String
scrollUp = printf "%cD" (toEnum 27 :: Char)

scrollDown :: String
scrollDown = printf "%cM" (toEnum 27 :: Char)

smoothScroll :: String
smoothScroll = printf "%c[?4h" (toEnum 27 :: Char)

setTopAndBottom :: Int -> Int -> String
setTopAndBottom = printf "%c[%d;%dr" (toEnum 27 :: Char) 

draw :: Int -> Int -> Int -> Int -> Buffer -> IO ()
draw topRow leftCol width height  b@Buffer{..} = do 

  let cls = printf "%c[2J" (toEnum 27 :: Char) ::String
      buffSlice = S.take height $ S.drop topRow content
  putStr cls
  putStr $ setTopAndBottom 0 10
  putStr $ toPos 0 0
  drawLines leftCol width buffSlice 
