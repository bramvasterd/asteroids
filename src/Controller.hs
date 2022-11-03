-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random

-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs (GameState p bs as rs st sd t) 
  = if st == Playing then return $ GameState (updatePlayer p) ( bs) ( as) ( rs) st (sd) (t+1)
    else return $ (GameState p bs as rs st sd t) 

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (SpecialKey KeyUp) Down _ _) (GameState (Player p m d a l) bs as rs st sd t)
  = GameState (Player p True d a l) bs as rs st sd t
inputKey (EventKey (SpecialKey KeyUp) Up _ _) (GameState (Player p m d a l) bs as rs st sd t)
  = GameState (Player p False d a l) bs as rs st sd t
inputKey (EventKey (SpecialKey KeyLeft) Down _ _) (GameState (Player p m d a l) bs as rs st sd t)
  = GameState (Player p m ToLeft a l) bs as rs st sd t
inputKey (EventKey (SpecialKey KeyLeft) Up _ _) (GameState (Player p m d a l) bs as rs st sd t)
  = GameState (Player p m Static a l) bs as rs st sd t
inputKey (EventKey (SpecialKey KeyRight) Down _ _) (GameState (Player p m d a l) bs as rs st sd t)
  = GameState (Player p m ToRight a l) bs as rs st sd t
inputKey (EventKey (SpecialKey KeyRight) Up _ _) (GameState (Player p m d a l) bs as rs st sd t)
  = GameState (Player p m Static a l) bs as rs st sd t
-- Otherwise keep the same
inputKey _ gstate = gstate

updatePlayer :: Player -> Player
updatePlayer (Player (x, y) m d a l) | m = Player (newX, newY) m d (updatePlayerAngle d a) l
                                     | otherwise = Player (x, y) m d (updatePlayerAngle d a) l
  where
    newX = x + sin(a) * 3 -- SPEEDFACTOR
    newY = y + cos(a) * 3 -- SPEEDFACTOR

updatePlayerAngle :: Direction -> Float -> Float
updatePlayerAngle d a | d == ToLeft  = a - 0.05
                      | d == ToRight = a + 0.05
                      | otherwise = a

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

updateRockets :: [Rocket] -> [Rocket]
updateRockets = undefined

updateRocket :: Rocket -> Rocket
updateRocket = undefined

updateSeed :: Int -> Int
updateSeed = undefined

checkState :: GameStatus
checkState = undefined

colliding :: Position -> Position -> Bool
colliding = undefined