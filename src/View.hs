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
      ShowScore world   -> drawScorescreen world
  


    -- Main draw function
    drawWorld :: World -> Picture
    drawWorld w = Pictures
        (drawAsteroids w ++
        drawbullets w ++ [drawPlane w] ++ drawScore w 
        )

    drawPlane :: World -> Picture
    --origineel 512x512 dus nu ong 64 bij 64
    drawPlane w@(World {player = p@(Player {playerlocation = (x,y), sprite = s})}) = (translate x y (scale 0.125 0.125 s))
    
    drawbullets :: World -> [Picture]
    drawbullets w@(World {bullets = listOfBullets}) = map drawbullet listOfBullets
    
    drawbullet :: Bullet -> Picture
    drawbullet Bullet { bulletLocation = (x,y), bulletStatus = s} = translate x y bullet
                                                          
    bullet :: Picture
    bullet = color red $ ThickCircle 1 2

    drawAsteroids :: World -> [Picture]
    drawAsteroids w@(World {asteroids = listOfAsteroids}) = map drawAsteroid listOfAsteroids

    drawAsteroid :: Asteroid -> Picture
    drawAsteroid Asteroid{ location = (x,y) , status = s, size = si} = translate x y (asteroid si)

    asteroid :: Float -> Picture
    asteroid a =  color white $ Line [(0*a,0*a), (5*a,4*a), (6*a,7*a), (4*a,9*a), (-3*a, 12*a), (-8*a,7*a),(-6*a,3*a),(0*a,0*a)]

    drawScore :: World -> [Picture]
    drawScore w@(World {score = s}) = [(scale 0.2 0.2 (translate 500 800 (color white $ Text (show s))))]
{-
    drawlives :: Float -> Float -> World -> [Picture]
    drawlives _ _ w@(World {lives = 0}) = []
    drawlives x y w@(World {lives = i, livespr = s}) = (drawlive x y s) : (drawlives (x+30) y (World {lives = i, livespr = s})) 
-}
    drawlive :: Float -> Float -> Picture -> Picture
    drawlive x y s = translate x y (scale 0.125 0.125 s)

  --score draw functions (for the death screen)
    drawScorescreen :: World -> Picture
    drawScorescreen = undefined


    