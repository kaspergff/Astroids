-- | This module defines how to turn
--   the game state into a picture
module View where

    import Graphics.Gloss
    import Model
    import Controller
    
    view :: GameState -> IO Picture
    view = return.viewPure
    
    viewPure :: GameState -> Picture
    viewPure gstate = case infoToShow gstate of
      ShowNothing       -> blank
      ShowWorld world   -> drawWorld world

      

    -- Main draw function
    drawWorld :: World -> Picture
    drawWorld w@(World {player = p@(Player {playerlocation = (x,y)})}) =  Pictures [translate x y drawPlane, drawbullets]

    drawPlane :: Picture
    drawPlane = Pictures[plane, planeNose]

    plane :: Picture
    plane = color white (Polygon [(-2, 10), (2, 10), (2, -15), (-2, -15), (-2, 10)])

    planeNose :: Picture
    planeNose = color yellow (Polygon [(-2, 15), (0,20), (2, 15), (2, 8), (-2, 8), (-2, 10)])

    bullet :: Picture
    bullet = color red $ ThickCircle 5 5

    drawbullets :: Picture
    drawbullets = Pictures [bullet]
