{-# LANGUAGE RecordWildCards #-}
module Main where

import Control.Concurrent
import Data.List as L
import Data.Sequence as S
import Data.Text as T
import Data.Text.IO as T
import Debug.Trace
import qualified System.Console.Terminal.Size  as TS
import System.Environment
import Prelude as P

import Keys
import TextBuffer
import TTYRender
import View


loadFile :: String -> IO BufferContent
loadFile fname = do
  text <- T.readFile fname
  return $ fromList $ T.lines text


updateBuffer :: Keys -> Buffer -> Buffer
updateBuffer k b = b' where
   b' = case k of 
    Alpha x -> insertCharacter x b
    Backspace -> deleteCharacter b
    CarriageReturn -> breakLine b
    CursorUp -> cursorUp b
    CursorDown -> cursorDown b
    CursorLeft -> cursorLeft b
    CursorRight -> cursorRight b
    End -> endOfLine b
    Home -> startOfLine b
    _ -> b

loop :: Buffer -> ViewState -> IO()
loop b v = do 
  key <- readKeys
  let b' = updateBuffer key b
  let v' = scrollView b' v
  P.putStr (toPos 30 0 ++  show (selection b'))
  draw v' b'
  loop b' v'


main :: IO ()
main = do
  env <- getEnvironment
  Just sz <- TS.size
  print env
  [inputFile] <- getArgs
  content <- loadFile inputFile
  let buffer' = Buffer content (Cursor 0 0 0) []
      offset = posToOffset buffer' 5 5
      selection = [Region (posToOffset buffer' 4 0) offset Selected]
      buffer = Buffer content (Cursor 0 0 0) selection
      view = ViewState 0 0 (TS.width sz) 20 -- (TS.height sz)
  initTTY
  loop buffer view
