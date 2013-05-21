module Constants where

import Data.Word (Word32)

toRadians :: Float -> Float
toRadians x = x/180*pi

blockSize :: Int
blockSize = 64

playerHeight :: Int
playerHeight = 32

fov :: Int
fov = 60

pPlaneDist, pPlaneH, pPlaneW :: Int
pPlaneDist = round $ fromIntegral (fst pPlaneCenter) / tan(toRadians(fromIntegral fov/2.0))
pPlaneH = screenHeight
pPlaneW = screenWidth

pPlaneCenter :: (Int,Int)
pPlaneCenter = (pPlaneW `div` 2, pPlaneH `div` 2)

pPlaneColWidth :: Float
pPlaneColWidth = fromIntegral fov / (fromIntegral pPlaneW)

screenHeight, screenWidth :: Int
screenHeight = 200
screenWidth = 320



word32Max :: Word32
word32Max = (maxBound :: Word32)
