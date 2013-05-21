module Level (
  Block(..)
 ,Level
 ,mkLevel
 )where

import qualified Constants as C

data Block = Wall | Empty deriving (Eq,Ord,Show)
data Level = Level [[Block]]

mkLevel :: [[Block]] -> Level
mkLevel = Level
