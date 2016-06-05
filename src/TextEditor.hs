-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE DuplicateRecordFields #-}

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
  getLayout r a _ = (r, Layout a)


data AppState = AppState { activeLayoutIndex :: Int 
                         , layout :: Root
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




newTop :: Int -> Int -> Int -> Int
newTop top height row | row >= top + height = row - height + 1
newTop top _ row | row < top = row 
newTop top _ _  = top 

scrollView :: EditBuffer -> Rect -> EditBuffer
scrollView a@(EditBuffer top left b) (Rect t l w h) = EditBuffer top' left' b where
  top' = newTop top h (line (cursor b))
  left' = newTop left w (col (cursor b))

gLayout :: AppState -> Root
gLayout = layout

loop :: AppState -> IO()
loop as@AppState{..} = do 
  key <- readKeys
  let (Root win rootLayout) = layout 
      (ar, ab) = getLayout win rootLayout activeLayoutIndex
      as' = case (fromLayout ab :: Maybe EditBuffer) of
        Just (EditBuffer row col b) -> as'' where 
          b'' = updateSelection $ updateBuffer key b{contentChanged = False}
          dirty = contentChanged b''
          b' = if dirty then  b''{regions  =  highLight (content b'')}  else b'' -- Still too often but better
          eb' = scrollView (EditBuffer row col b') ar
  -- Update the buffer ref in the layout
          layout' = updateRoot layout activeLayoutIndex (Layout eb')
          as'' = as{layout = layout'}
        Nothing -> error "reference to none buffer"
  -- P.putStr (toPos 30 0 ++  show (selection b'))
  layoutRoot (gLayout as')
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
      window = Rect 0 0 (TS.width sz) 30
      layout = Root window $ Layout (HSplit 14 (Layout (EditBuffer 0 0 buffer)) (Layout (EditBuffer 0 0 buffer)))
      as = AppState 2 layout bufferManager

  case layout of 
    Root r l -> do 
      print l 
      print $ getLayout r l 0
      print $ getLayout r l 1
      print $ getLayout r l 2
  -- error "Ow"
  
  loop as
  putStrLn "Hello"



