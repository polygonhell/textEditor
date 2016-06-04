{-# LANGUAGE RecordWildCards #-}
module View where

import TextBuffer

data ViewState = ViewState { bTop   :: Int
                           , bLeft  :: Int
                           , top    :: Int
                           , left   :: Int
                           , width  :: Int
                           , height :: Int
}


newTop :: Int -> Int -> Int -> Int
newTop top height row | row >= top + height = row - height + 1
newTop top _ row | row < top = row 
newTop top _ _  = top 

scrollView2 :: Buffer -> ViewState -> ViewState
scrollView2 Buffer{..} v@ViewState{..} = v' where
  Cursor{..} = cursor
  v' = v{top = newTop top height line, left = newTop left width col}
