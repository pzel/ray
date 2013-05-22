module Level (
  Block(..)
 ,Level
 ,LevelPos
 ,LevelGPos
 ,(!)
 ,(!#)
 ,mkLevel
 ,mkSquareLevel 
 ) where

import Globals (blockSize)

data Block = Wall | Empty deriving (Eq,Ord,Show)
data Level = Level [[Block]] deriving (Show)

type LevelPos = (UnitCoord,UnitCoord)
type LevelGPos = (GridCoord,GridCoord)
type UnitCoord = Float
type GridCoord = Int

mkLevel :: [[Block]] -> Level
mkLevel = Level

mkSquareLevel :: Int -> Level
mkSquareLevel s = Level [ replicate s Wall, 
                          ([Wall]++(replicate (s-2) Empty)++[Wall]),
                          replicate s Wall ]

infixl 9 ! 
infixl 9 !#

(!) :: Level -> LevelPos -> Block
(Level l)!(xf,yf) = let x = (round xf) `div` blockSize
                        y = (round yf) `div` blockSize
                    in if (min x y) >= 0 && y < length l && x < length (l!!y)
                       then (l!!y)!!x
                       else Wall

(!#) :: Level -> LevelGPos -> Block
(Level l)!#(x,y) = if (min x y) >= 0 && y < length l && x < length (l!!y)
                   then (l!!y)!!x
                   else Wall