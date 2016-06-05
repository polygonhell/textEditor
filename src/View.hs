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


