module Main where

import Data.Sequence

import TextBuffer
import TTYRender


multiLineContent :: BufferContent
multiLineContent = fromList ["This is a test", "Dogs and Cats Living Together", "Purple rain falling", "Complete drivel", "And another line to act as a test", "short line"]

multiLineBuffer = Buffer multiLineContent (Cursor 0 0 0)

main :: IO ()
main = draw 0 0 8 3 multiLineBuffer
