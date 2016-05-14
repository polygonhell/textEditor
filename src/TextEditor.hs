{-# LANGUAGE RecordWildCards #-}

module TextEditor where 


import qualified System.Console.Terminal.Size  as TS
import qualified Data.Sequence as S
import qualified Data.Map as M
import TextBuffer


data BufferManager = BufferManager {textBuffers :: M.Map String Buffer}

-- This view is just the Top left offset of the Buffer, the buffer Size is dictated by the window Splits
data View = View Int Int
data WindowManager = AppWindow { active :: Int  -- Active Window index by left recursive walk
                               , width :: Int 
                               , height :: Int 
                               , child :: WindowManager }       
                   | VSplit { line :: Int
                            , top :: WindowManager 
                            , bottom :: WindowManager }
                   | HSplit { col :: Int 
                            , left :: WindowManager
                            , right :: WindowManager } 
                   | Window { buffer :: String -- Buffer by name 
                            , view :: View }
                   | StatusLine

data AppState = AppState { appBufferManager :: BufferManager
                         , appWindowManager :: WindowManager
                         }


newTextEditor :: IO AppState
newTextEditor = do
  Just sz <- TS.size
  let emptyBuffer = Buffer S.empty (Cursor 0 0 0) [] 0 [] False []
      bm = BufferManager $ M.singleton "**empty" emptyBuffer
      mainVS = VSplit ((TS.height sz) -2) (Window "**empty" (View 0 0)) StatusLine
      wm = AppWindow 0 (TS.width sz) (TS.height sz) mainVS
  return $ AppState bm wm


getActiveWindow' :: Int -> Int -> WindowManager -> (WindowManager, Int)
getActiveWindow' i target wm@VSplit{..} = (wm', i') where 
  (t, ti) = getActiveWindow' (i) target top
  (b, bi) = getActiveWindow' (ti+1) target bottom
  (wm', i') = 
    if ti == target 
      then (t, ti)
      else (b, bi)  
-- Windows with no children count by index
getActiveWindow' i target wm  = (wm, i)

getActiveWindow :: AppState -> WindowManager
getActiveWindow as@AppState{..} = wm where 
  AppWindow {..} = appWindowManager
  wm = fst $ getActiveWindow' 0 active child

bufferByName :: String -> AppState -> Buffer
bufferByName name as@AppState{..} = b where 
  BufferManager {..} = appBufferManager
  b = textBuffers M.! name