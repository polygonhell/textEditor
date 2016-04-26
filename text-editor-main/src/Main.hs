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
import Highlight


loadFile :: String -> IO BufferContent
loadFile fname = do
  text <- T.readFile fname
  return $ fromList $ T.lines text


updateBuffer :: Keys -> Buffer -> Buffer
updateBuffer k b@Buffer{..} = b' where
   b' = case k of 
    Alpha x -> insertCharacter x b
    Backspace | P.null selection -> deleteCharacter b
    Backspace -> deleteSelection b
    CarriageReturn -> breakLine b
    CursorUp -> cursorUp b
    CursorDown -> cursorDown b
    CursorLeft -> cursorLeft b
    CursorRight -> cursorRight b
    End -> endOfLine b
    Home -> startOfLine b
    Ctrl 'a' -> startSelection b
    _ -> b

loop :: Buffer -> ViewState -> IO()
loop b v = do 
  key <- readKeys
  let b'' = updateSelection $ updateBuffer key b{contentChanged = False}
      dirty = contentChanged b''
      b' = if dirty then  b''{regions  =  highLight (content b'')}  else b'' -- Still too often but better
      v' = scrollView b' v
  P.putStr (toPos 30 0 ++  show (selection b'))
  draw v' b'
  loop b' v'


testContent :: BufferContent
testContent = S.fromList [pack " -- This is a \"test\"", pack "\"Not a comment\"", pack "  -- This is a test "]

main :: IO ()
main = do
  env <- getEnvironment
  Just sz <- TS.size
  print env
  [inputFile] <- getArgs
  content <- loadFile inputFile
  let selection = []
      -- regions = [Region (posToOffset buffer' 7 7) (posToOffset buffer' 7 19) [Comment]]
      regions = highLight content
      buffer = Buffer content (Cursor 0 0 0) selection 0 regions False
      view = ViewState 0 0 (TS.width sz) 20 -- (TS.height sz)
  -- print $ highLight testContent
  -- print "Hello"
  initTTY
  loop buffer view
