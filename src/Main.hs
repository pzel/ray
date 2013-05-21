module Main where

import qualified Constants as C
import Graphics.UI.SDL as SDL
import Level (Block(..),Level,mkLevel)


main :: IO ()
main = do
  SDL.init [ InitEverything ]
  _ <- SDL.setVideoMode C.screenWidth C.screenHeight 8 []
  screen <- SDL.getVideoSurface
  let level = mkLevel [[Wall, Wall, Wall, Wall],
                       [Wall,Empty,Empty,Empty],
                       [Wall,Empty,Empty,Empty],
                       [Wall, Wall, Wall, Wall]]
  mainLoop screen level
  SDL.quit
 where
  mainLoop s l = do
   _ <- drawLevel s l
   e <- SDL.pollEvent
   l' <- updateLevel l
   case e of
    (KeyUp (Keysym SDLK_q _ _)) -> return ()
    _         -> mainLoop s l'

drawLevel :: Surface -> Level -> IO Bool
drawLevel screen _ = do
  fillRect screen (Just (Rect 0 0 320 240)) (Pixel 78990) >> SDL.tryFlip screen

updateLevel :: Level -> IO Level
updateLevel = return