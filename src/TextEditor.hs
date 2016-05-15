-- {-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ExistentialQuantification #-}


module TextEditor where 


-- import qualified System.Console.Terminal.Size  as TS
-- import qualified Data.Sequence as S
import qualified Data.Map as M
-- import Data.Traversable
import TextBuffer


data BufferManager = BufferManager {textBuffers :: M.Map String Buffer}

data SplitDirection = Horizontal



data Layout a where
  Layout :: (LayoutClass a) =>  a -> Layout a

-- data Layout a = forall l. (LayoutClass l a) => Layout (l a)

class LayoutClass a where
  doLayout :: Rect -> a -> IO () 



-- top left width height 
data Rect  = Rect Int Int Int Int




data Empty = Empty

data Root where
  Root ::(LayoutClass a) => Rect -> a -> Root

data HSplit where
  HSplit :: (LayoutClass a, LayoutClass b) => Int -> a -> b -> HSplit 


instance LayoutClass Empty where
  doLayout _ _ = putStr "Empty"

instance LayoutClass HSplit where
  doLayout r (HSplit _ a b) = do 
    doLayout r a
    doLayout r b
    putStr "Empty"


test :: Root
test = Root (Rect 0 0 20 20) (HSplit 5 Empty Empty)

layout :: Root -> IO()
layout (Root r lo) = doLayout r lo
