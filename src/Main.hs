module Main where

import Data.Sequence
import Data.List as L
import Debug.Trace
import Control.Concurrent

import TextBuffer
import TTYRender
import View


multiLineContent :: BufferContent
multiLineContent =  fromList $ concat $ L.replicate 10 ["This is a test", "Dogs and Cats Living Together", "Purple rain falling", "Complete drivel", "And another line to act as a test", "short line"]

multiLineBuffer = Buffer multiLineContent (Cursor 2 5 2)
initialViewState = ViewState 0 0 100 10


updateBuffer:: String -> Buffer -> Buffer
updateBuffer k b = b' where
   b' = case k of 
    "Up" -> cursorUp b
    "Down" -> cursorDown b
    "Left" -> cursorLeft b
    "Right" -> cursorRight b
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
  -- draw initialViewState multiLineBuffer
  -- threadDelay (10*1000000)
  loop multiLineBuffer initialViewState
