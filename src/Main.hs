module Main where

import Control.Monad (zipWithM)
import Data.Angle
import Data.Maybe (isJust,fromJust)
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
   distances <- mapM (castRay l lp lp) angles -- unfishbowlize
   rects <- zipWithM projection distances [0..]
   mapM_ (\r-> SDL.fillRect s r (Pixel 12566463)) rects
   return ()

projection :: Float -> Int -> IO (Maybe Rect)
projection d col =
  let height = (round $ (fromIntegral G.blockSize / d) * 277)::Int
      top = max 0 $ (G.pPlaneH `div` 2) - (height `div` 2)
  in return $ Just $ Rect col top 1 (height)

castRay :: Level -> LevelPos -> LevelPos -> Angle -> IO Float
castRay l p@(px,py) c@(cx,cy) a =
  let dx = (cos' a) * 3
      dy = (sin' a) * 3
      md = checkWall l p c
  in do 
      if isJust md
         then return (fromJust md)
         else castRay l p (cx+dx, cy+dy) a

checkWall :: Level -> LevelPos -> LevelPos -> Maybe Float
checkWall l p cu@(checkX,checkY) =
    case l!cu of
      Wall  -> Just $ getDistance p cu
      Empty -> Nothing

getDistance (x1,y1) (x2,y2) = sqrt (((x1-x2)^^2) + ((y1-y2)^^2))

cos' = cos . toRadians
sin' = sin . toRadians