-- | This module defines how to turn
--   the game state into a picture
module View where

    import Graphics.Gloss
    import Model
    import Controller

    view :: GameState -> IO Picture
    view gstate@(GameState {world = w@(World {score = points})}) = do
      let p = viewPure gstate
      case infoToShow gstate of
          ShowDeathscreen w -> highScoreScreen p
          _ -> return p
    
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
        drawbullets w ++ [drawPlane w] ++ drawScore w ++ drawLives w 
        )

    drawPlane :: World -> Picture
    --origineel 512x512 dus nu ong 64 bij 64
    drawPlane w@(World {player = p@(Player {playerLocation = (x,y)}), sprites = s}) = (translate x y (scale 0.125 0.125 (s!!0)))
    
    drawEnemies :: World -> [Picture]
    drawEnemies w@(World {enemies = listOfEnemies, sprites = s}) = map (drawEnemy (s!!2)) listOfEnemies
    
    drawEnemy :: Picture -> Enemy -> Picture
    drawEnemy  p (Enemy {enemyLocation = (x,y)})= (translate x y (scale 0.100 0.100 (p)))

    drawbullets :: World -> [Picture]
    drawbullets w@(World {bullets = listOfBullets}) = map drawbullet listOfBullets
    
    drawbullet :: Bullet -> Picture
    drawbullet Bullet { bulletLocation = (x,y), bulletStatus = s} = translate x y bullet
                                                          
    bullet :: Picture
    bullet = color blue $ ThickCircle 1 2

    drawAsteroids :: World -> [Picture]
    drawAsteroids w@(World {asteroids = listOfAsteroids}) = map drawAsteroid listOfAsteroids

    drawAsteroid :: Asteroid -> Picture
    drawAsteroid Asteroid{ asteroidLocation = (x,y) , asteroidStatus = s, asteroidSize = si} = translate x y (asteroid si)

    asteroid :: Float -> Picture
    asteroid a =  color white $ Line [(0*a,0*a), (5*a,4*a), (6*a,7*a), (4*a,9*a), (-3*a, 12*a), (-8*a,7*a),(-6*a,3*a),(0*a,0*a)]

    drawScore :: World -> [Picture]
    drawScore w@(World {score = s}) = [(scale 0.2 0.2 (translate 500 800 (color white $ Text (show s))))]

    drawLives :: World -> [Picture]
    drawLives w@(World {lives = l, sprites = s}) = drawLife (-180) (170) l (s!!1)

    drawLife :: Float -> Float -> Int -> Picture -> [Picture]
    drawLife x y l s | (l < 1) == True = []
                     | otherwise = [(translate x y (scale (0.125/2) (0.125/2) s))] ++ drawLife (x+40) y (l-1) s

    -- Draws the final screen
    drawDeathscreen :: World -> Picture
    drawDeathscreen w@(World {score = points}) = setHighScoreText (setGameOver (drawFinalPoints w))

    drawFinalPoints :: World -> Picture 
    drawFinalPoints w@(World {score = points}) = (translate (-100) 100 (scale 0.2 0.2 (color white $ Text ("You scored: " ++ (show points) ++ " points"))))

    setGameOver :: Picture -> Picture
    setGameOver p = Pictures ([p] ++ [(translate (-100) 130 (scale 0.2 0.2 (color white $ Text ("Game Over!"))))])

    setHighScoreText :: Picture -> Picture
    setHighScoreText p = Pictures([p] ++ [(translate (-100) 70 (scale 0.2 0.2 (color white $ Text ("Highscores: "))))])

        -- Reads the highscores from the file and draws them on the screen
    highScoreScreen :: Picture -> IO Picture
    highScoreScreen p = do
      highscores <- getScoreFromTXT
      let amount = length highscores
          inPlace = hScoreOnScreen highscores amount
          hSscreen = Pictures ([p] ++ inPlace)
      return hSscreen

    hScoreOnScreen :: [String] -> Int -> [Picture]
    hScoreOnScreen s 0 = [] 
    hScoreOnScreen s n = (translate (-100) (setScoreD n) (scale 0.2 0.2 (color white $ Text((s !! (n-1)) ++ " ")))) : hScoreOnScreen s (n-1)

    setScoreD :: Int -> Float
    setScoreD x = (-200) + 50 * fromIntegral x
