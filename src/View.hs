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
    drawWorld w@(World {player = p@(Player {playerlocation = (x,y)})}) = Pictures
        (drawAsteroids w ++
        [translate x y drawPlane] ++ drawbullets w ++ drawScore w
        )

    drawPlane :: Picture
    drawPlane = Pictures[plane, planeNose]

    plane :: Picture
    plane = color white (Polygon [(-2, 10), (2, 10), (2, -15), (-2, -15), (-2, 10)])

    planeNose :: Picture
    planeNose = color yellow (Polygon [(-2, 15), (0,20), (2, 15), (2, 8), (-2, 8), (-2, 10)])


    drawbullets :: World -> [Picture]
    drawbullets w@(World {bullets = listOfBullets}) = map drawbullet listOfBullets
    
    drawbullet :: Bullet -> Picture
    drawbullet Bullet { bulletLocation = (x,y)} = translate x y bullet
                                                          
    
    bullet :: Picture
    bullet = color red $ ThickCircle 5 5

    drawAsteroids :: World -> [Picture]
    drawAsteroids w@(World {asteroids = listOfAsteroids}) = map drawAsteroid listOfAsteroids

    drawAsteroid :: Asteroid -> Picture
    drawAsteroid Asteroid{ location = (x,y) , status = s} 
        | s == NotDestroyed = translate x y asteroid
        | otherwise = Blank

    asteroid :: Picture
    asteroid =  color white $ ThickCircle 5 5

    drawScore :: World -> [Picture]
    drawScore w@(World {score = s}) = [(scale 0.2 0.2 (translate 800 800 (Text (show s))))]