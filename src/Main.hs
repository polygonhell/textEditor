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
import System.Posix.Signals

import Keys
import TextBuffer
import TTYRender
import View
import Highlight
import qualified TextEditor as TE
import qualified Layout as L

-- TODO - Cut and Paste
-- TODO - Window Manager
-- TODO - Status Line
-- TODO - Custom key bindings
-- TODO - Deal with Empty files
-- TODO - Save





loadFile :: String -> IO BufferContent
loadFile fname = do
  text <- T.readFile fname
  return $ fromList $ T.lines text



testContent :: BufferContent
testContent = S.fromList [pack " -- This is a \"test\"", pack "\"Not a comment\"", pack "  -- This is a test "]

main :: IO ()
main = do
  env <- getEnvironment
  -- Prevent Ctrl-Z from suspending the app
  let signalSet = addSignal sigTSTP emptySignalSet
  blockSignals signalSet

  Just sz <- TS.size
  print env
  [inputFile] <- getArgs
  content <- loadFile inputFile
  let selection = []
      -- regions = [Region (posToOffset buffer' 7 7) (posToOffset buffer' 7 19) [Comment]]
      regions = highLight content
      buffer = Buffer content (Cursor 0 0 0) selection 0 regions False []
      view = ViewState 0 0 0 0 (TS.width sz) 20 -- (TS.height sz)
  -- print $ highLight testContent
  -- print "Hello"
  initTTY
  TE.init [("buffer", buffer)]

