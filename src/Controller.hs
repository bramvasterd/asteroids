-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs (GameState p bs as rs st sd t) = 
  do randomNumber <- randomIO
    if state == Playing then return $ GameState (updatePlayer p) (updateBullets bullets) (updateAsteroids as) (updateRockets rs) checkState (updateSeed seed) (t+1)
    else return $ gstate { elapsedTime = elapsedTime gstate + secs }

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyUp) Down _ _) (GameState (Player p m a l) bs as rs st sd t)
  = GameState (Player p True a l) bs as rs st sd t
inputKey (EventKey (SpecialKey KeyUp) Up _ _) (GameState (Player p m a l) bs as rs st sd t)
  = GameState (Player p False a l) bs as rs st sd t
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) (GameState (Player p m a l) bs as rs st sd t)
  = GameState (Player p False a l) bs as rs st sd t
inputKey (EventKey (SpecialKey KeyLeft) Up _ _) (GameState (Player p m a l) bs as rs st sd t)
  = GameState (Player p False a l) bs as rs st sd t
inputKey (EventKey (SpecialKey KeyRight) Down _ _) (GameState (Player p m a l) bs as rs st sd t)
  = GameState (Player p False a l) bs as rs st sd t
inputKey (EventKey (SpecialKey KeyRight) Up _ _) (GameState (Player p m a l) bs as rs st sd t)
  = GameState (Player p False a l) bs as rs st sd t
-- Otherwise keep the same
inputKey _ gstate = gstate

updatePlayer :: Player -> Player
updatePlayer (Player (x, y) m a l) | m = Player (newX, newY) m a l
                                   | otherwise = Player (x, y) m a l
  where
    newX = x + sin(a) * 1 -- SPEEDFACTOR
    newY = y + cos(a) * 1 -- SPEEDFACTOR

updateBullets :: [Bullet] -> [Bullet]
updateBullets = undefined

updateBullet :: Bullet -> Bullet
updateBullet = undefined

updateAsteroids :: [Asteroid] -> [Asteroid]
updateAsteroids = undefined

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid = undefined

spawnAsteroid :: [Asteroid] -> Int -> [Asteroid]
spawnAsteroid = undefined

keepAsteroid :: Asteroid -> Bool
keepAsteroid = undefined

updateRockets :: [Rockets] -> [Rockets]
updateRockets = undefined

updateRocket :: Rocket -> Rocket
updateRocket = undefined

checkState :: GameState
checkState = undefined

colliding :: Position -> Position -> Bool
colliding = undefined