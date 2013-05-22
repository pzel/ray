module Globals (
  blockSize
 ,playerHeight
 ,fov ,halfFov
 ,pPlaneDist,pPlaneH,pPlaneW,pPlaneCenter,pPlaneColWidth
 ,screenHeight,screenWidth
 ,word32Max
 ) where

import Data.Angle
import Data.Word (Word32)

blockSize :: Int
blockSize = 64

playerHeight :: Int
playerHeight = 32

fov :: Angle
fov = deg 60

halfFov :: Angle
halfFov = deg $ fov / 2.0

pPlaneDist, pPlaneH, pPlaneW :: Int
pPlaneDist = round $ fromIntegral (fst pPlaneCenter) / tan(toRadians(fov/2.0))

pPlaneH = screenHeight
pPlaneW = screenWidth

pPlaneCenter :: (Int,Int)
pPlaneCenter = (pPlaneW `div` 2, pPlaneH `div` 2)

pPlaneColWidth :: Float
pPlaneColWidth = fov / (fromIntegral pPlaneW)

screenHeight, screenWidth :: Int
screenHeight = 200
screenWidth = 320

word32Max :: Word32
word32Max = (maxBound :: Word32)
