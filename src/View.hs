-- | This module defines how to turn
--   the game state into a picture
module View where

import Graphics.Gloss
import Model

view :: GameState -> IO Picture
view = return . viewInitialstate

viewInitialstate :: GameState -> IO Picture
view _ =  do 
  let x = -300
  let y = 100
  let itext = translate x y  (scale 0.3 0.3 (color white (text ("Welcome to alien defence" ))))
  let initialtext = picture itext
  return initialtext


