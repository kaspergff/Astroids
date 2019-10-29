-- | This module defines how to turn
--   the game state into a picture
module View where

    import Graphics.Gloss
    import Model
    
    view :: GameState -> IO Picture
    view = return . viewPure
    
    viewPure :: GameState -> Picture
    viewPure gstate = case infoToShow gstate of
      ShowNothing       -> blank
      ShowWorld world   -> drawWorld world


    -- Main draw function
    drawWorld :: World -> Picture
    drawWorld w = drawPlane

    drawPlane :: Picture
    drawPlane = color white (Polygon [(-2, 10), (2, 10), (2, -15), (-2, -15), (-2, 10)])