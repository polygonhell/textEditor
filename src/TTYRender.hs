{-# LANGUAGE RecordWildCards #-}
module TTYRender where

import Control.Concurrent
import Control.Monad
import Data.Char
import Data.Foldable
import Data.Sequence as S
import Data.Text as T
import Debug.Trace
import System.IO
import System.Posix.IO (fdRead, stdInput, stdOutput)
import System.Posix.Terminal
import Text.Printf

import TextBuffer
import View
import Keys


-- TODO this needs state for the following
-- TODO hide the cursor during update
-- TODO use the cosoles scroll functionality
-- TODO only redraw changed lines

drawLines :: Int -> Int -> BufferContent -> IO ()
drawLines _ _ b | b == S.empty = return ()
drawLines leftCol width b = do
  let h :< t = viewl b
      line = T.take width $ T.drop leftCol h
      padding = T.replicate (width - T.length line) $ T.singleton ' '
      str = printf "%s%s" line padding :: String
  putStr str
  when (t /= S.empty) $ putStr "\n"
  drawLines leftCol width t

cls :: String
cls = "\ESC[2J"

toPos :: Int -> Int -> String
toPos = printf "\ESC[%d;%dH"

index :: String
index = "\ESCD"

revIndex :: String
revIndex = "\ESCM"

smoothScroll :: String
smoothScroll = "\ESC[?4h"

setTopAndBottom :: Int -> Int -> String
setTopAndBottom = printf "\ESC[%d;%dr"

resetTopAndBottom :: String
resetTopAndBottom = "\ESC[r"

hideCursor :: String
hideCursor = "\ESC[?25l"

showCursor :: String
showCursor = "\ESC[?25h"


printable :: Char -> Bool
printable x = (fromEnum x :: Int) >= 0x20

readKeys :: IO Keys
readKeys = do
  (str, bytes) <- fdRead stdInput 3 
  return $ case str of
    "\ESC[A" -> CursorUp
    "\ESC[B" -> CursorDown
    "\ESC[C" -> CursorRight
    "\ESC[D" -> CursorLeft
    "\ESC[F" -> End
    "\ESC[H" -> Home
    "\x7f" -> Backspace  -- Delete key on OSX Keyboard
    "\n" -> CarriageReturn
    [a] | printable a -> Alpha a
    _ -> trace ("Keys pressed -- " ++ str) UnknownKey


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
  putStr hideCursor
  putStr $ toPos 0 0
  drawLines left width buffSlice 

  let cursorX = col cursor - left + 1
      cursorY = line cursor - top + 1
  putStr $ toPos cursorY cursorX
  putStr showCursor
  hFlush stdout




