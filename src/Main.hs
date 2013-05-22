module Main where

import Data.Angle
import qualified Globals as G
import Graphics.UI.SDL as SDL hiding (flip)
import Level (Block(..),Level,LevelPos,(!),mkLevel)

main :: IO ()
main = do
  SDL.init [ InitEverything ]
  SDL.setVideoMode G.screenWidth G.screenHeight 8 []
  screen <- SDL.getVideoSurface
  let level = mkLevel [[Wall, Wall, Wall, Wall],
                       [Wall,Empty,Empty,Empty],
                       [Wall,Empty,Empty,Empty],
                       [Wall, Wall, Wall, Wall]]

  mainLoop screen level
  SDL.quit
 where
  mainLoop s l = do
   drawLevel s l
   return ()
   -- e <- SDL.pollEvent
   -- case e of
   --  (KeyUp (Keysym SDLK_q _ _)) -> return ()
   --  _                           -> mainLoop s l

drawLevel :: Surface -> Level -> IO Bool
drawLevel screen l = do
  fillRect screen (Just (Rect 0 0 320 240)) (Pixel 1)
  -- the player is at (1,2):gridCoord or (96,160):unitCoord in the level
  castRays screen l (96.0,160.0) (deg 45.0)
  SDL.tryFlip screen

castRays :: Surface -> Level -> LevelPos -> Angle -> IO ()
castRays s l lp facing = 
 do
  let angles = take G.pPlaneW $ iterate (\a->deg(G.pPlaneColWidth+a)) (facing - G.halfFov)
      distances = map (castRay l lp) angles
  putStrLn (show distances) >> return ()

castRay :: Level -> LevelPos -> Angle -> Float
castRay l p a  = let hdist = findHIntersection l p a
                     vdist = findVIntersection l p a
                 in min hdist vdist

findHIntersection, findVIntersection :: Level -> LevelPos -> Angle -> Float
findHIntersection l p@(px,py) a = 
  let bSize = (fromIntegral G.blockSize)::Float
      ay = (py/bSize) * bSize + (if upP a then (-1.0) else bSize)
      ax = px + (py-ay)/tan(G.alpha)
      ya = if upP a then (-bSize) else bSize
      xa = bSize/tan(a)
  in checkWall l p (ax,ay) (xa,ya)

checkWall :: Level -> LevelPos -> LevelPos -> (Float,Float) -> Float
checkWall l p@(px,py) c@(checkX,checkY) a@(xa,ya) =
  if l!c == Wall
  then getDistance p c
  else checkWall l p (checkX+xa, checkY+ya) a

getDistance (x1,y1) (x2,y2) = sqrt $ (x1-x2)^2 + (y1-y2)^2
findVIntersection l p a = 99999.9
