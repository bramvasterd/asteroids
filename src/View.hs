-- | This module defines how to turn
--   the game state into a picture
module View where

import Data.List
import Data.Ord
import Graphics.Gloss
import Model



view:: GameState -> IO Picture 
view state = do
  playerpic <- (viewPlayer state)
  bulletpic <- (viewBullet state)
  asteroidpic <- (viewAsteroids state)
  rocketpic <- (viewRockets state)
  return (pictures [playerpic, bulletpic, asteroidpic, rocketpic])

viewPlayer :: GameState -> IO Picture
viewPlayer (GameState (Player (x,y) _ alpha _) _ _ _ _ _ _ )= do 
            playerbmp <- loadBMP "src/spaceship.bmp"
            let scaleplayer = scale 0.3 0.3 playerbmp
            let rotateplayer  = rotate (alpha*180/pi) scaleplayer
            let player = translate x y rotateplayer
            return player

viewBullet :: GameState -> IO Picture
viewBullet (GameState _ bs _ _ _ _ _ ) = do 
            let bulletbmp = Color (white) (circleSolid 1)
            let shots = pictures [translate x y bulletbmp | Bullet (x,y) _ <- bs ]
            return shots

viewAsteroids :: GameState -> IO Picture
viewAsteroids (GameState _ _ as _ _ _ _ ) = do
            asteroidbmp <- loadBMP "src/asteroidbmp.bmp"
            let asteroids = pictures [translate x y (scale size size asteroidbmp) | Asteroid (x,y) size <- as]
            return asteroids

viewRockets :: GameState -> IO Picture
viewRockets (GameState _ _ _ rs _ _ _) = do 
            rocketbmp <- loadBMP "src/rocket.bmp"
            let rockets = pictures [translate x y (scale 1 1 rocketbmp) | Rocket (x,y) _ <- rs]
            return rockets


