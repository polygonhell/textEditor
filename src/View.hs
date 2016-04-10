{-# LANGUAGE RecordWildCards #-}
module View where

import TextBuffer

data ViewState = ViewState { top    :: Int
                           , left   :: Int
                           , width  :: Int
                           , height :: Int
}


newTop :: Int -> Int -> Int -> Int
newTop top height row | row >= top + height = top + 1 
newTop top _ row | row < top = top - 1 
newTop top _ _  = top 

scrollView :: Buffer -> ViewState -> ViewState
scrollView Buffer{..} v@ViewState{..} = v' where
  Cursor{..} = cursor
  v' = v{top = newTop top height line}
