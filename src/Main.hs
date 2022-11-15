module Main where

import Controller
import Model
import View

import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Color

main :: IO ()
main = playIO (InWindow "Counter" (1000, 1000) (0, 0)) -- Or FullScreen
              black            -- Background color
              60               -- Frames per second
              initialState     -- Initial state
              view             -- View function
              input            -- Event function
              step             -- Step function