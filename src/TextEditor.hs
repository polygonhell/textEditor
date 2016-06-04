-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE DuplicateRecordFields #-}

module TextEditor where 


import qualified System.Console.Terminal.Size  as TS
-- import qualified Data.Sequence as S
import qualified Data.Map as M
import System.Posix.Signals

import TextBuffer
import Prelude as P
import View
import Layout
import Highlight
import Keys
import TTYRender

data BufferManager = BufferManager {textBuffers :: M.Map String Buffer}

-- top left buffer offset
data EditBuffer = EditBuffer Int Int Buffer

instance Show EditBuffer where
  show _ = "EditBuffer"

instance LayoutClass EditBuffer where
  doLayout (Rect t l w h) (EditBuffer row col b) = do
    -- Layout is 0 based, TTY renderer is 1 based
    let viewState = ViewState row col (t+1) (l+1) w h
    draw viewState b
  layoutSize _ = 1
  updateLayout _ 0 n = n
  updateLayout a _ _ = Layout a


data AppState = AppState { activeLayoutIndex :: Int 
                         , activeBuffer :: EditBuffer
                         , layout :: Root
                         , window :: Rect
                         , bufferManager :: BufferManager 
                         } 


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
    Alt 'a' -> startSelection b
    Alt _ -> undo b
    Ctrl 'z' -> undo b
    _ -> b


scrollView :: EditBuffer -> EditBuffer
scrollView a = a

loop :: AppState -> IO()
loop as@AppState{..} = do 
  key <- readKeys
  let (EditBuffer row col b) = activeBuffer
      b'' = updateSelection $ updateBuffer key b{contentChanged = False}
      dirty = contentChanged b''
      b' = if dirty then  b''{regions  =  highLight (content b'')}  else b'' -- Still too often but better
      eb' = scrollView $ EditBuffer row col b'
  -- Update the buffer ref in the layout
      layout' = updateRoot layout activeLayoutIndex (Layout eb')

      as' = as{activeBuffer = eb', layout = layout'}
  -- P.putStr (toPos 30 0 ++  show (selection b'))
  layoutRoot layout'
  loop as'




init :: [(String, Buffer)] -> IO()
init buffers = do
  -- env <- getEnvironment
  -- Prevent Ctrl-Z from suspending the app
  let signalSet = addSignal sigTSTP emptySignalSet
  blockSignals signalSet
  initTTY

  Just sz <- TS.size

  let bufferManager = BufferManager (M.fromList buffers)
      buffer = snd $ head buffers
      window = Rect 0 0 (TS.width sz) 25
      layout = Root window $ Layout (HSplit 20 (Layout (EditBuffer 0 0 buffer)) (Layout Empty))
      as = AppState 1 (EditBuffer 0 0 buffer) layout window bufferManager
  loop as
  putStrLn "Hello"



