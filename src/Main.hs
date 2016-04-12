module Main where

import Data.Sequence as S
import Data.List as L
import Data.Text as T
import Debug.Trace
import Control.Concurrent

import Keys
import TextBuffer
import TTYRender
import View




multiLineContent :: BufferContent
multiLineContent =  fromList $ L.concat $ L.replicate 10 $ L.map pack ["This is a test", "Dogs and Cats Living Together, this is a longer line", "Purple rain falling", "Complete drivel", "And another line to act as a test", "short line"]

multiLineBuffer = Buffer multiLineContent (Cursor 2 5 2)
initialViewState = ViewState 0 0 40 10


updateBuffer:: Keys -> Buffer -> Buffer
updateBuffer k b = b' where
   b' = case k of 
    Alpha x -> insertCharacter x b
    Backspace -> deleteCharacter b
    CarriageReturn -> breakLine b
    CursorUp -> cursorUp b
    CursorDown -> cursorDown b
    CursorLeft -> cursorLeft b
    CursorRight -> cursorRight b
    _ -> b

loop :: Buffer -> ViewState -> IO()
loop b v = do 
  -- print b
  key <- readKeys
  let b' = updateBuffer key b
  let v' = scrollView b' v
  draw v' b'
  loop b' v'


main :: IO ()
main = do
  initTTY
  loop multiLineBuffer initialViewState
