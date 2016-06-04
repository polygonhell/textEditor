-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}

module Layout where 

import Text.Printf
import TextBuffer

data Layout where
  Layout :: (Show a, LayoutClass a) =>  a -> Layout

class LayoutClass a where
  doLayout :: (LayoutClass a, Show a) => Rect -> a -> IO ()
  layoutSize :: a -> Int 
  updateLayout ::  a -> Int -> Layout -> Layout 

-- top left width height 
data Rect  = Rect Int Int Int Int deriving Show

splitHorizontal :: Int -> Rect -> (Rect, Rect)
splitHorizontal row (Rect t l w h) = (top, bottom) where
  row' = min row (h-1)
  top = Rect t l w row'
  bottom = Rect (row'+1) l w (h-(row'+1))

data Empty = Empty

data Root = Root Rect Layout

data HSplit = HSplit Int Layout Layout



instance Show Root where
  show (Root r a) = printf "Root %s [%s]" (show r) (show a)

instance Show HSplit where
  show (HSplit i a b) = printf "HSplit %d [%s][%s]" i (show a) (show b)

instance Show Empty where
  show _ = "Empty"

instance Show Layout where
  show (Layout a) = printf "W(%s)" $ show a

instance LayoutClass Layout where
  doLayout r (Layout a) = doLayout r a
  layoutSize (Layout a) = layoutSize a
  updateLayout (Layout a) n new = updateLayout a n new

instance LayoutClass Empty where
  doLayout _ _ = return ()
  layoutSize _ = 1
  updateLayout _ 0 n = n
  updateLayout _ _ _ = Layout Empty


instance LayoutClass HSplit where
  doLayout r (HSplit row a b) = do 
    let (tr, br) = splitHorizontal row r 
    doLayout tr a
    doLayout br b
  layoutSize (HSplit _ a b) = 1 + (layoutSize a) + (layoutSize b)
  updateLayout _ 0 n = n
  updateLayout (HSplit row a b) n new = Layout (HSplit row a' b') where
    s = layoutSize a
    a' = if n-1 <= layoutSize a then updateLayout a (n-1) new else a
    b' = if n-1 <= layoutSize a then b else updateLayout b (n-(1+s)) new



-- u2 :: Layout -> Int -> Layout -> Layout

test :: Root
test = Root (Rect 0 0 20 40) $ Layout (HSplit 35 (Layout Empty) (Layout Empty))

layoutRoot :: Root -> IO()
layoutRoot (Root r lo) = doLayout r lo

updateRoot :: Root -> Int -> Layout -> Root
updateRoot (Root r lo) i l = Root r lo' where
  lo' = updateLayout lo i l