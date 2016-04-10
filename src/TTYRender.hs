{-# LANGUAGE RecordWildCards #-}
module TTYRender where

import Control.Concurrent
import Data.Foldable
import Data.List as L
import Data.Sequence as S
import System.IO
import System.Posix.IO (fdRead, stdInput, stdOutput)
import System.Posix.Terminal
import Text.Printf

import TextBuffer
import View


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


readKeys :: IO String
readKeys = do
  (str, bytes) <- fdRead stdInput 3 
  return $ case str of
    "\ESC[A" -> "Up"
    "\ESC[B" -> "Down"
    "\ESC[C" -> "Right"
    "\ESC[D" -> "Left"
    _ -> "ow"


initTTY :: IO ()
initTTY = do 
  oldTermSettings <- getTerminalAttributes stdInput
  {- modify settings -}
  let newTermSettings = flip withoutMode  EnableEcho   . -- don't echo keystrokes
                        flip withoutMode  ProcessInput $ -- turn on non-canonical mode
                        oldTermSettings
    -- flip withTime     vtime        . -- wait at most vtime decisecs per read
    -- flip withMinInput vmin         $ -- wait for >= vmin bytes per read

  setTerminalAttributes stdInput newTermSettings Immediately

  -- application `finally` setTerminalAttributes stdInput oldTermSettings Immediately
  
  putStrLn "Inited" 

draw :: ViewState -> Buffer -> IO ()
draw ViewState{..} b@Buffer{..} = do 

  let buffSlice = S.take height $ S.drop top content
  putStr cls
  -- putStr $ setTopAndBottom 0 5
  putStr resetTopAndBottom
  putStr $ toPos 0 0
  drawLines left width buffSlice 

  -- putStr $ toPos 0 0
  -- putStr (concat $ L.replicate 3 revIndex )
  let cursorX = col cursor - left + 1
      cursorY = line cursor - top + 1
  putStr $ toPos cursorY cursorX
  -- putStr $ toPos 0 0
  hFlush stdout
  -- threadDelay $ 10 * 1000000




