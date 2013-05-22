module Main where

import Control.Monad (zipWithM)
import Data.Angle
import qualified Globals as G
import Graphics.UI.SDL as SDL hiding (flip)
import Level

main :: IO ()
main = do
  SDL.init [ InitEverything ]
  SDL.enableKeyRepeat 1 1
  SDL.setVideoMode G.screenWidth G.screenHeight 8 []
  screen <- SDL.getVideoSurface
  let level = mkSquareLevel 17
  mainLoop screen level (65,65) 280
  SDL.quit
 where
  mainLoop s l p@(px,py) a = do
   SDL.pumpEvents
   drawLevel s l p a
   e <- SDL.pollEvent
   case e of
    (KeyDown (Keysym SDLK_q _ _))    -> return ()
--    (KeyDown (Keysym SDLK_UP _ _)) -> mainLoop s l (px,py+8) (deg (a-1.0))
    (KeyDown (Keysym SDLK_LEFT _ _)) -> mainLoop s l p (deg (a-3.0))
    (KeyDown (Keysym SDLK_RIGHT _ _)) -> mainLoop s l p (deg (a+3.0))
    _                              -> mainLoop s l p a

drawLevel :: Surface -> Level -> LevelPos -> Angle -> IO Bool
drawLevel screen l p a = do
  fillRect screen (Just (Rect 0 0 320 200)) (Pixel 1)
  castRays screen l p a
  SDL.tryFlip screen

castRays :: Surface -> Level -> LevelPos -> Angle -> IO ()
castRays s l lp facing =
 do
   let angles = take G.pPlaneW $ iterate (\a->deg(G.pPlaneColWidth+a)) (facing - G.halfFov)
       distances = map (castRay l lp) angles -- unfishbowlize
   rects <- zipWithM projection distances [0..]
   mapM_ (\r-> SDL.fillRect s r (Pixel 12566463)) rects
   return ()

projection :: Float -> Int -> IO (Maybe Rect)
projection d col =
  let height = (round $ (fromIntegral G.blockSize / d) * 277)::Int
      top = max 0 $ (G.pPlaneH `div` 2) - (height `div` 2)
  in do putStrLn (show ("col",col,"dist",d,"height",height,"top",top))
        return $ Just $ Rect col top 1 (height)

castRay :: Level -> LevelPos -> Angle -> Float
castRay l p a  = let hdist = findHIntersection l p a
                     vdist = findVIntersection l p a
                 in min vdist hdist

toGrid :: LevelPos -> LevelGPos
toGrid (x,y) = (floor(x/64), floor(y/64))

findHIntersection, findVIntersection :: Level -> LevelPos -> Angle -> Float
findHIntersection l p@(px,py) a =
  let bSize = (fromIntegral G.blockSize)::Float
      cy = (py/bSize) * bSize + (if upP a then (-1.0) else bSize)
      cx = px + (py-cy)/tan a
      ya = if upP a then (-bSize) else bSize
      xa = bSize/tan a
      checkUnit = (cx,cy)
      checkGrid = toGrid checkUnit
  in checkWall l p checkUnit checkGrid (xa,ya) a

findVIntersection l p@(px,py) a =
  let bSize = (fromIntegral G.blockSize)::Float
      cx = (px/bSize) * bSize + (if rightP a then bSize else (-1.0))
      cy = py + (px-cx)*tan a
      xa = if rightP a then bSize else (-bSize)
      ya = bSize*tan a
      checkUnit = (cx,cy)
      checkGrid = toGrid checkUnit
  in checkWall l p checkUnit checkGrid (xa,ya) a

checkWall :: Level -> LevelPos -> LevelPos -> LevelGPos -> (Float,Float) -> Angle -> Float
checkWall l p@(px,_) cu@(checkX,checkY) cg a@(xa,ya) ang =
  case l!#cg of 
    Wall  -> getTrigDist px checkX ang
    Empty -> checkWall l p (checkX+xa, checkY+ya) (toGrid (checkX+xa, checkY+ya)) a ang

getTrigDist :: Float -> Float -> Angle -> Float 
getTrigDist x1 x2 a = abs(x1-x2) / cos a

---getDistfance (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2
