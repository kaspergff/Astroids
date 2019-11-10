-- | This module defines how to turn
--   the game state into a picture
module View where

    import Graphics.Gloss
    import Model
    import Controller

    -- -- Main view function om de game te laten zien
    -- highScoreScreen p wordt gebruikt om de naam van de speler te laten invullen
    view :: GameState -> IO Picture
    view gstate@(GameState {world = w@ World {score = points}}) = do
      let p = viewPure gstate
      case infoToShow gstate of
          ShowDeathscreen w -> highScoreScreen p
          _ -> return p
    
     -- De verschillende schermen (ShowNothing vooral voor debuggen)     
    viewPure :: GameState -> Picture
    viewPure gstate = case infoToShow gstate of
      ShowNothing       -> blank
      ShowWorld world   -> drawWorld world
      ShowDeathscreen world  -> drawDeathscreen world

    -- Main draw function tekent alle onderdelen
    drawWorld :: World -> Picture
    drawWorld w = Pictures
        (drawAsteroids w ++
         drawEnemies w ++
        drawbullets w ++ 
        [drawPlane w] ++ 
        drawScore w ++ 
        drawLives w ++ 
        drawRocks w
        )

    -- Tekent de player    
    drawPlane :: World -> Picture
    --origineel 512x512 dus nu ong 64 bij 64
    drawPlane World {player = p@(Player {playerLocation = (x,y)}), sprites = (s:sx)} = translate x y (scale 0.125 0.125 (s))
    -- Tekent alle enemies
    drawEnemies :: World -> [Picture]
    drawEnemies World {enemies = listOfEnemies, sprites = s} = map (drawEnemy (s!!2)) listOfEnemies
    -- Tekent een enkele enemy
    drawEnemy :: Picture -> Enemy -> Picture
    drawEnemy  p Enemy {enemyLocation = (x,y)}= translate x y (scale 0.100 0.100 p)
-- tekent alle bullets
    drawbullets :: World -> [Picture]
    drawbullets World {bullets = listOfBullets} = map drawbullet listOfBullets
    -- tekent één bullet
    drawbullet :: Bullet -> Picture
    drawbullet Bullet { bulletLocation = (x,y), bulletStatus = s} = translate x y bullet
    -- hoe ziet een bullet eruit                                                      
    bullet :: Picture
    bullet = color blue $ ThickCircle 1 2
    -- Tekent alle asteroids
    drawAsteroids :: World -> [Picture]
    drawAsteroids World {asteroids = listOfAsteroids} = map drawAsteroid listOfAsteroids
    -- tekent één asterioid
    drawAsteroid :: Asteroid -> Picture
    drawAsteroid Asteroid{ asteroidLocation = (x,y), asteroidSize = si} = translate x y (asteroid si)
      -- hoe ziet een asteroid eruit
    asteroid :: Float -> Picture
    asteroid a =  color white $ Line [(0*a,0*a), (5*a,4*a), (6*a,7*a), (4*a,9*a), (-3*a, 12*a), (-8*a,7*a),(-6*a,3*a),(0*a,0*a)]
    -- tekent alle rocks die worden gebruikt bij destructie asteroid
    drawRocks :: World -> [Picture]
    drawRocks World {rocks = listOfRocks} = map drawRock listOfRocks
      -- tekent één rock
    drawRock :: Rock -> Picture
    drawRock Rock{rockLocation = (x,y)} = translate x y (scale 0.5 0.5 rock)
      --hoe ziet een rock eruit
    rock :: Picture
    rock = color white $ Line [(0,0), (5,4), (6,7), (4,9), (-3, 12), (-8,7),(-6,3),(0,0)]
      -- tekent de huidige score op het scherm
    drawScore :: World -> [Picture]
    drawScore World {score = s} = [scale 0.2 0.2 (translate 500 800 (color white $ Text (show s)))]
      -- tekent de levens
    drawLives :: World -> [Picture]
    drawLives World {lives = l, sprites = s} = drawLife (-180) 170 l (s!!1)
      -- tekent 1 hartje
    drawLife :: Float -> Float -> Int -> Picture -> [Picture]
    drawLife x y l s | (l < 1) = []
                     | otherwise = (translate x y (scale (0.125/2) (0.125/2) s)) : drawLife (x+40) y (l-1) s

    --Tekent het eindscherm
    drawDeathscreen :: World -> Picture
    drawDeathscreen w@(World {score = points}) = setHighScoreText (setGameOver (drawFinalPoints w))
      -- Tekent het behaalde aantal punten
    drawFinalPoints :: World -> Picture 
    drawFinalPoints World {score = points} = translate (-100) 100 (scale 0.2 0.2 (color white $ Text ("You scored: " ++ show points ++ " points")))
-- zet game over op het scherm
    setGameOver :: Picture -> Picture
    setGameOver p = Pictures (p : [translate (-100) 130 (scale 0.2 0.2 (color white $ Text "Game Over!"))])
-- laat de highscores zien
    setHighScoreText :: Picture -> Picture
    setHighScoreText p = Pictures(p : [translate (-100) 70 (scale 0.2 0.2 (color white $ Text "Highscores: "))])

        -- Leest de highscores van de .txt file
    highScoreScreen :: Picture -> IO Picture
    highScoreScreen p = do
      highscores <- getScoreFromTXT
      let amount = length highscores
          inPlace = hScoreOnScreen highscores amount
          hSscreen = Pictures (p : inPlace)
      return hSscreen

    -- zet de highscores op het scherm
    hScoreOnScreen :: [String] -> Int -> [Picture]
    hScoreOnScreen s 0 = [] 
    hScoreOnScreen s n =   translate (-100) (setScoreD n) (scale 0.2 0.2 (color white $ Text ((s !! (n - 1)) ++ " "))): hScoreOnScreen s (n - 1)
      -- hulp functie voor tekenen highscore
    setScoreD :: Int -> Float
    setScoreD x = (-200) + 50 * fromIntegral x
