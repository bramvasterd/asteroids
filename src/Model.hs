-- | This module contains the data types
--   which represent the state of the game
module Model where
type Angle    = Float
type Velocity = Float
type Score    = Float
type Position = (Float, Float)

data Asteroid   = Asteroid  {asteroidPosition:: Position, size :: Float}
data Player     = Player    {playerPosition:: Position, moving :: Bool, playerDirection :: Direction, playerAngle:: Angle, lives::Int}
data Bullet     = Bullet    {bulletPosition:: Position, bulletAngle :: Float}
data Rocket     = Rocket    {rocketPosition:: Position, rocketAngle :: Float}

data Direction  = ToLeft | ToRight | Static
                  deriving (Eq)

data GameStatus = Initial | Playing | Paused | Stopped
    deriving Eq





nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                            player :: Player,
                            bullets :: [Bullet],
                            asteroids :: [Asteroid],
                            rockets :: [Rocket],
                            state :: GameStatus ,
                            seed :: Int,
                            elapsedTime :: Float}

initialState :: GameState
initialState = GameState (Player (0, 0) False Static 0 3) [] [] [] Playing 1 0