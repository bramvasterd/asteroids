-- | This module contains the data types
--   which represent the state of the game
module Model where

type Angle    = Float
type Velocity = Float
type Score    = Float
type Position = (Float, Float)

data Asteroid   = Asteroid {position:: Position, size :: Float}
data Player     = Player {position:: Position, shooting:: Bool, playerAngle:: Angle, lives::Int}
data Bullet     = Bullet {velocity:: Velocity, angle:: Angle}
data Rocket     = Rocket {position:: Position}

data GameStatus = Initial | Playing | Paused | Stopped





nO_SECS_BETWEEN_CYCLES :: Float
nO_SECS_BETWEEN_CYCLES = 5

data GameState = GameState {
                            player :: Player
                            bullets :: [Bullet]
                            asteroids :: [Asteroid]
                            rockets :: [Rockets]
                            state :: GameStatus 
                            seed :: Int
                            elapsedTime :: Float}

initialState :: GameState
initialState = GameState ShowNothing 0