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
      ShowDeathscreen world  -> drawDeathscreen world
  


    -- Main draw function
    drawWorld :: World -> Picture
    drawWorld w = Pictures
        (drawAsteroids w ++
         drawEnemies w ++
         drawenemytimer w ++
        drawbullets w ++ drawrockets w ++ [drawPlane w] ++ drawScore w ++ drawLives w 
        )

    drawPlane :: World -> Picture
    --origineel 512x512 dus nu ong 64 bij 64
    drawPlane w@(World {player = p@(Player {playerlocation = (x,y)}), sprites = s}) = (translate x y (scale 0.125 0.125 (s!!1)))
    
    drawEnemies :: World -> [Picture]
    drawEnemies w@(World {enemies = listOfEnemies, sprites = s}) = map (drawEnemy (s!!2)) listOfEnemies
    
    drawEnemy :: Picture -> Enemy -> Picture
    drawEnemy  p (Enemy {enemyLocation = (x,y)})= (translate x y (scale 0.100 0.100 (p)))

    
    drawbullets :: World -> [Picture]
    drawbullets w@(World {bullets = listOfBullets}) = map drawbullet listOfBullets
    
    drawbullet :: Bullet -> Picture
    drawbullet Bullet { bulletLocation = (x,y), bulletStatus = s} = translate x y bullet
                                                          
    bullet :: Picture
    bullet = color red $ ThickCircle 1 2

    drawrockets :: World -> [Picture]
    drawrockets w@(World {rockets = listOfRockets}) = map drawrocket listOfRockets

    drawrocket :: Rocket -> Picture
    drawrocket Rocket {rockLocation = (x,y), rocketStatus = s} = translate x y rocket 

    rocket :: Picture
    rocket = color green $ Polygon [(-1,10),(1,10),(1,0),(-1,0),(-1,5)]

    drawAsteroids :: World -> [Picture]
    drawAsteroids w@(World {asteroids = listOfAsteroids}) = map drawAsteroid listOfAsteroids

    drawAsteroid :: Asteroid -> Picture
    drawAsteroid Asteroid{ location = (x,y) , status = s, size = si} = translate x y (asteroid si)

    asteroid :: Float -> Picture
    asteroid a =  color white $ Line [(0*a,0*a), (5*a,4*a), (6*a,7*a), (4*a,9*a), (-3*a, 12*a), (-8*a,7*a),(-6*a,3*a),(0*a,0*a)]

    drawScore :: World -> [Picture]
    drawScore w@(World {score = s}) = [(scale 0.2 0.2 (translate 500 800 (color white $ Text (show s))))]

    drawLives :: World -> [Picture]
    drawLives w@(World {lives = l, sprites = s}) = drawLife (-180) (170) l (s!!1)

    drawLife :: Float -> Float -> Int -> Picture -> [Picture]
    drawLife x y l s | (l < 1) == True = []
                     | otherwise = [(translate x y (scale (0.125/2) (0.125/2) s))] ++ drawLife (x+40) y (l-1) s

--debug

    drawenemytimer :: World -> [Picture]
    drawenemytimer w@(World {enemyTimer = s}) = [(scale 0.2 0.2 (translate 200 600 (color green $ Text (show s))))]





  --score draw functions (for the death screen)
    drawDeathscreen :: World -> Picture
    drawDeathscreen  w = Pictures (drawScore' w ++ supporttext)

    supporttext :: [Picture]
    supporttext = [p1] ++  [p2] ++  [p3]
      where 
        p1 = (translate (-100) 100 (scale 0.2 0.2 (color white $ Text ("your score is : "))))
        p2 = (translate (-200) (-50) (scale 0.15 0.15 (color white $ Text ("do you wish to save your score?"))))
        p3 = (translate (-200) (-100) (scale 0.15 0.15 (color white $ Text ("press ' s ' to save and ' x ' to close the window "))))

    drawScore' :: World -> [Picture]
    drawScore' w@(World {score = s}) = [(translate (-50) 0(scale 0.8 0.8 (color white $ Text (show s))))]


    