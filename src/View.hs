-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Data.Monoid
import Graphics.Gloss.Data.Bitmap
import Model
import System.IO
import Control.Monad
import System.Directory


view:: GameState -> IO Picture 
view (GameState p bs as rs st sd t)  = 
  if (st == Playing) then do
    playerpic <- (viewPlayer p)
    bulletpic <- (viewBullet bs)
    asteroidpic <- (viewAsteroids as)
    rocketpic <- (viewRockets rs)
    scorepic <- (viewScore t)
    return (pictures [playerpic, bulletpic, asteroidpic, rocketpic, scorepic])
  else if (st == Paused) then do 
    playerpic <- (viewPlayer p)
    bulletpic <- (viewBullet bs)
    asteroidpic <- (viewAsteroids as)
    rocketpic <- (viewRockets rs)
    scorepic <- (viewScore t)
    let pausetext = translate (-200) (-10) (scale 0.2 0.2 (color white (text ("PAUSED \n Press P to start")))) 
    return (pictures [playerpic, bulletpic, asteroidpic, rocketpic, scorepic, pausetext])
  else do
    let finishText = translate (-180) (100) (scale 0.2 0.2 (color white (text ("GAME OVER")))) 
    sc <- (viewScore t)
    let scoreFin = translate (-450) (-300) sc
    return (pictures [finishText, scoreFin])

viewPlayer :: Player -> IO Picture
viewPlayer (Player (x,y) _ _ alpha _) = do 
            playerbmp <- loadBMP "src/spaceship.bmp"
            let scaleplayer = scale 0.3 0.3 playerbmp
            let rotateplayer  = rotate (alpha*180/pi) scaleplayer
            let player = translate x y rotateplayer
            return player

viewBullet :: [Bullet] -> IO Picture
viewBullet bs = do 
            let bulletbmp = Color (white) (circleSolid 3)
            let shots = pictures [translate x y bulletbmp | Bullet (x,y) _ <- bs ]
            return shots

viewAsteroids :: [Asteroid] -> IO Picture
viewAsteroids as = do
            asteroidbmp <- loadBMP "src/asteroidbmp.bmp"
            let asteroids = pictures [translate x y (scale (size*0.2) (size*0.2) asteroidbmp) | Asteroid (x,y) size _ <- as]
            return asteroids

viewRockets :: [Rocket] -> IO Picture
viewRockets rs = do 
            rocketbmp <- loadBMP "src/rocket.bmp"
            let rockets = pictures [translate x y (scale 0.2 0.2 (rotate (a*180/pi) rocketbmp)) | Rocket (x,y) a <- rs]
            return rockets

viewScore :: Float -> IO Picture 
viewScore  t = do 
            let score  = (round t) `div` 100
            hs <- highscore score
            let scoreText = translate 250 320 (scale 0.2 0.2 (color white (text ("High Score: " ++ hs ++ "  Score: " ++ show score)))) 
            return scoreText

highscore :: Int -> IO String
highscore score = do 
            t <- readFile "highscore.txt" 
            let x = read t :: Int
            let y = if score > x then score else x
            let xs = show y
            writeFile "highscore2.txt" xs     -- Cant overwrite the original .txt file due to lazy evaluation
            removeFile "highscore.txt"        -- Solution is to make a new file, remove old one and rename the new file
            renameFile "highscore2.txt" "highscore.txt"
            return xs


