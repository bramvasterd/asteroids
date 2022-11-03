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
  = 
       if st == Playing then return $ GameState (updatePlayer p) (updateBullets bs) (updateAsteroids as sd) ( rs) st (updateSeed sd) (t+1)
       else if st == Paused then return $ GameState p bs as rs st sd t
       else if st== Initial then return $ GameState p bs as rs st sd t
       else return $ (GameState p [] [] [] Stopped sd t)

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

inputKey :: Event -> GameState -> GameState
inputKey (EventKey (Char 'w') Down _ _) (GameState (Player p m d a l) bs as rs st sd t)
  = GameState (Player p True d a l) bs as rs st sd t
inputKey (EventKey (Char 'w') Up _ _) (GameState (Player p m d a l) bs as rs st sd t)
  = GameState (Player p False d a l) bs as rs st sd t
inputKey (EventKey (Char 'a') Down _ _) (GameState (Player p m d a l) bs as rs st sd t)
  = GameState (Player p m ToLeft a l) bs as rs st sd t
inputKey (EventKey (Char 'a') Up _ _) (GameState (Player p m d a l) bs as rs st sd t)
  | d == ToLeft = GameState (Player p m Loose a l) bs as rs st sd t
  | otherwise = GameState (Player p m d a l) bs as rs st sd t
inputKey (EventKey (Char 'd') Down _ _) (GameState (Player p m d a l) bs as rs st sd t)
  = GameState (Player p m ToRight a l) bs as rs st sd t
inputKey (EventKey (Char 'd') Up _ _) (GameState (Player p m d a l) bs as rs st sd t)
  | d == ToRight = GameState (Player p m Loose a l) bs as rs st sd t
  | otherwise = GameState (Player p m d a l) bs as rs st sd t
inputKey (EventKey (SpecialKey KeySpace) Down _ _) (GameState (Player (x,y) m d a l) bs as rs st sd t)
 = GameState (Player (x,y) m d a l) ((Bullet (x + 40 * sin(a), y + 40 * cos(a)) a):bs) as rs st sd t

inputKey (EventKey (Char 'p') Down _ _) (GameState p bs as rs st sd t)
  = if st == Initial || st == Paused then GameState p bs as rs Playing sd t
    else if st == Playing then GameState p bs as rs Paused sd t
    else GameState p bs as rs st sd t

inputKey (EventKey (Char 'q') Down _ _) (GameState p bs as rs st sd t)
 = GameState p bs as rs Stopped sd t

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
updateBullets bs = filter (\(Bullet (x, y) a) -> abs x < 500 || abs y < 500) [updateBullet b | b <- bs]

updateBullet :: Bullet -> Bullet
updateBullet(Bullet (x, y) a) = Bullet (newX, newY) a
  where 
    newX = x + sin(a) * 6 -- BULLETSPEED
    newY = y + cos(a) * 6 -- BULLETSPEED

updateAsteroids :: [Asteroid] -> Int -> [Asteroid]
updateAsteroids as sd | sd `mod` 100 == 1 = (spawnAsteroid sd):as
                      | otherwise = filter (keepAsteroid) [updateAsteroid a | a <- as]

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid (Asteroid (x,y) s a) = (Asteroid (newX, newY) s a)
  where
    newX = x + sin(a / 10) * 2 -- ASTEROIDSPEED
    newY = y + cos(a / 10) * 2 -- ASTEROIDSPEED

spawnAsteroid :: Int -> Asteroid
spawnAsteroid sd = (Asteroid (xLoc,yLoc) 2 a)
  where
    cs = [-500..500]
    (xLoc,yLoc,a) | s == 0 = (-1000, cs !! i, fst $ randomR (5, 20) (mkStdGen sd))
                  | s == 1 = (1000, cs !! i, fst $ randomR (40, 55) (mkStdGen sd))
                  | s == 2 = (cs !! i, -1000, fst $ randomR (-5, 10) (mkStdGen sd))
                  | s == 3 = (cs !! i, 1000, fst $ randomR (20, 35) (mkStdGen sd))
                  -- | otherwise = (-320, cs !! i, fst $ randomR (5, 20) (mkStdGen sd))
    s = (fst $ randomR (0, 3) (mkStdGen sd)) :: Int
    i = (fst $ randomR (0, 1000) (mkStdGen (sd + 1))) :: Int

keepAsteroid :: Asteroid -> Bool
keepAsteroid a = True

updateRockets :: [Rocket] -> [Rocket]
updateRockets = undefined

updateRocket :: Rocket -> Rocket
updateRocket = undefined

updateSeed :: Int -> Int
updateSeed seed | seed < 5000 = seed + 1
                | otherwise = 0 

checkState :: GameStatus
checkState = undefined

colliding :: Position -> Position -> Bool
colliding = undefined