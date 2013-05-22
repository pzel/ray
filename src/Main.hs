module Main where

import Control.Applicative
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
  level <- parseLevel <$> readFile "data/l0.lev" 
  mainLoop screen level (65,65) 45
  SDL.quit
 where
  mainLoop s l p@(px,py) a = do
   SDL.pumpEvents
   drawLevel s l p a
   e <- SDL.pollEvent
   case e of
    (KeyDown (Keysym SDLK_q _ _))    -> return ()
    (KeyDown (Keysym SDLK_UP _ _)) -> mainLoop s l (moveByAngle 4 l p a) a
    (KeyDown (Keysym SDLK_DOWN _ _)) -> mainLoop s l (moveByAngle 4 l p (deg(a+180.0))) a
    (KeyDown (Keysym SDLK_LEFT _ _)) -> mainLoop s l p (deg (a-2.0))
    (KeyDown (Keysym SDLK_RIGHT _ _)) -> mainLoop s l p (deg (a+2.0))
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
       distances = map (castRay l lp lp) angles
       correctedDistances = (zipWith (correct facing) angles distances)
   rects <- zipWithM projection correctedDistances [0..]
   mapM_ (\r-> SDL.fillRect s r (Pixel 12566463)) rects
   return ()

correct :: Angle -> Angle -> Float -> Float
correct facing ray dist = max 1 (dist * (cos' (deg (ray - facing))))

projection :: Float -> Int -> IO (Maybe Rect)
projection  d col =
  let height = (round $ (fromIntegral G.blockSize / d) * 277)::Int
      top = max 0 $ (G.pPlaneH `div` 2) - (height `div` 2)
  in return $ Just $ Rect col top 1 (height)

castRay :: Level -> LevelPos -> LevelPos -> Angle -> Float
castRay l p@(px,py) c@(cx,cy) a = 
  if newC == c
  then getDistance p c
  else castRay l p newC a
 where newC = moveByAngle 4 l c a 

moveByAngle :: Float -> Level -> LevelPos -> Angle -> LevelPos
moveByAngle s l p@(px,py) a = 
  let dx = (cos' a) * s
      dy = (sin' a) * s
      newP = (px+dx,py+dy)
  in if (isWall l newP) then p else newP

isWall :: Level -> LevelPos -> Bool
isWall l p = l!p == Wall

getDistance (x1,y1) (x2,y2) = sqrt (((x1-x2)^^2) + ((y1-y2)^^2))

cos' = cos . toRadians
sin' = sin . toRadians