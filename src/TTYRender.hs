{-# LANGUAGE RecordWildCards #-}
module TTYRender where

import Control.Concurrent
import Data.Foldable
import Data.Char
import Data.List as L
import Data.Sequence as S
import System.IO
import System.Posix.IO (fdRead, stdInput, stdOutput)
import System.Posix.Terminal
import Text.Printf
import Debug.Trace

import TextBuffer
import View
import Keys


drawLines :: Int -> Int -> BufferContent -> IO ()
drawLines _ _ b | b == S.empty = return ()
drawLines leftCol width b = do
  let h :< t = viewl b
      line = L.take width $ L.drop leftCol h
      padding = L.replicate (width - L.length line) ' '
      str = printf "%s%s\n" line padding :: String
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


alpha :: Char -> Bool
alpha x = isAlphaNum x || x == ' ' 

readKeys :: IO Keys
readKeys = do
  (str, bytes) <- fdRead stdInput 3 
  return $ case str of
    "\ESC[A" -> CursorUp
    "\ESC[B" -> CursorDown
    "\ESC[C" -> CursorRight
    "\ESC[D" -> CursorLeft
    "\n" -> CarriageReturn
    [a] | alpha a -> Alpha a
    _ -> UnknownKey


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
  -- putStr cls
  -- putStr $ setTopAndBottom 0 5
  putStr resetTopAndBottom
  putStr $ toPos 0 0
  drawLines left width buffSlice 

  let cursorX = col cursor - left + 1
      cursorY = line cursor - top + 1
  putStr $ toPos cursorY cursorX
  hFlush stdout




