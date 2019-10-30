-- | This module defines how the state changes
--   in response to time and user input
module Controller where

    import Model
    
    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    import System.Random
    import System.Environment
    

    -- | Handle one iteration of the game
    step :: Float -> GameState -> IO GameState
    step secs gstate  
        | (isPaused $ world gstate) == True = return $ gstate {infoToShow = ShowWorld $ world gstate, world = world gstate}
        | otherwise = return $ gstate { elapsedTime = elapsedTime gstate + secs, infoToShow = ShowWorld(updateWorld $ world gstate), world = updateWorld $ world gstate}
    
    -- | Handle user input
    input :: Event -> GameState -> IO GameState
    input e gstate = return (inputKey e gstate)


    -- Key input
    inputKey :: Event -> GameState -> GameState                                             
    inputKey (EventKey (SpecialKey (KeyLeft)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = LeftMovement}}}
    inputKey (EventKey (SpecialKey (KeyRight)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = RightMovement}}}
    inputKey (EventKey (SpecialKey (KeyUp)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = UpMovement}}}
    inputKey (EventKey (SpecialKey (KeyDown)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = DownMovement}}}    
    --bullet
    inputKey (EventKey (SpecialKey (KeySpace)) _ _ _ ) gstate@(GameState _ w@(World {bullets = l}) _) = gstate {world = (spawnBullet w)}
    
    inputKey (EventKey (Char ('p')) _ _ _) gstate@(GameState{world = w}) = gstate {world = w {pause = Paused} }
    inputKey (EventKey (Char ('r')) _ _ _) gstate@(GameState{world = w}) = gstate {world = w {pause = Playing} }
    inputKey _ gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = NoMovement}}}
    
    updateWorld :: World -> World
    updateWorld w = 
        updatePlane $ 
        asteroidPlayer $
        planeOnScreen $ 
        updateBullets $
        scoreChecker $
        updateAsteroids $ 
        timeToSpawnAsteroid $
        asteroidBullet w

    -- Move plane
    updatePlane :: World -> World

    updatePlane w@(World {player = p@(Player {playerlocation = (x,y), movement = dir})})
        | dir == RightMovement = w{ player = p {playerlocation = (x + 11 ,y), movement = RightMovement}}
        | dir == LeftMovement = w{ player = p {playerlocation = (x - 11,y), movement = LeftMovement}}
        | dir == DownMovement = w{ player = p {playerlocation = (x,y - 11), movement = DownMovement}}
        | dir == UpMovement = w{ player = p {playerlocation = (x,y + 11), movement = UpMovement}}
        | dir == NoMovement = w

    -- Keep plane on screen
    planeOnScreen :: World -> World
    planeOnScreen w@(World {player = p@(Player {playerlocation = (x,y)})})
        | x < (-200) = w{ player = p {playerlocation = (-200,y)}}
        | x > (200) = w{ player = p {playerlocation = (200,y)}}
        | y < (-190) = w{ player = p {playerlocation = (x,-190)}}
        | y > (170) = w{ player = p {playerlocation = (x,170)}}
        | otherwise = w 

    isPaused :: World -> Bool
    isPaused w@(World {pause = paused})
        | paused == Playing = False
        | paused == Paused = True    

    -- Update all asteroids    
    updateAsteroids :: World -> World
    updateAsteroids w@(World {asteroids = listOfAsteroids}) = w{asteroids = map updateAsteroid listOfAsteroids}

    updateAsteroid :: Asteroid -> Asteroid
    updateAsteroid a@(Asteroid{ location = (x,y)}) = a{ location = (x,y-2)}

    spawnAsteroid :: World -> World
    spawnAsteroid w@(World {asteroids = listOfAsteroids, asteroidsSpawnGenerator = g }) = w{asteroids = listOfAsteroids ++ [createAsteroid (setRanNum g)],
    asteroidsSpawnGenerator = getGen (genereerRanNum g) }

    createAsteroid :: Float -> Asteroid
    createAsteroid x  = Asteroid (x,200) NotDestroyed

    timeToSpawnAsteroid :: World -> World
    timeToSpawnAsteroid w@(World {asteroidTimer = time}) 
        | time < 1 = spawnAsteroid w{ asteroidTimer = 40}
        | otherwise = w {asteroidTimer = time - 1}

    genereerRanNum :: StdGen -> (Float, StdGen)
    genereerRanNum g = randomR ((-175), 175) g

    getRanNum :: (Float, StdGen) -> Float
    getRanNum (x, _) = x

    getGen :: (Float, StdGen) -> StdGen
    getGen (_, g) = g
    
    setRanNum :: StdGen -> Float
    setRanNum g = getRanNum (genereerRanNum g)
    
    updateBullets :: World -> World
    updateBullets w@(World {bullets = listOfBullets}) = w{bullets = map updateBullet listOfBullets}

    updateBullet :: Bullet -> Bullet
    updateBullet b@(Bullet{ bulletLocation = (x,y), speed = s}) = b{ bulletLocation = (x,(y+s)), speed = s}

    spawnBullet :: World -> World
    spawnBullet w@(World {player = p@(Player {playerlocation = (x,y)}), bullets = listOfBullets}) = w{bullets = listOfBullets ++ [(createBullet (x,y))] }

    createBullet :: (Float,Float) -> Bullet
    createBullet (x,y) = (Bullet (x,y) 20)

    --collision checkers
    --asteroid and bullet

    collisionAsteroidBullet :: Asteroid -> Bullet -> Bool
    collisionAsteroidBullet a@(Asteroid {location = (ax,ay), status = s}) b@(Bullet {bulletLocation= (bx,by)})
        | ax >= (bx-15) && ax <= (bx+15) && ay >= (by-15) && ay <= (by+15) = True
        | otherwise = False

    asteroidBullet :: World -> World
    asteroidBullet w@(World {asteroids = []}) = w
    asteroidBullet w@(World {asteroids = listOfAsteroids, bullets = listOfBullets, score = s}) = w{asteroids = map check listOfAsteroids}
            where
                check asteroid | all (==False) (map (collisionAsteroidBullet asteroid) listOfBullets) == True = asteroid
                               | otherwise = asteroid{status = Destroyed}   
    --player and asteroid
    collisionAsteroidplayer :: Player -> Asteroid -> Bool
    collisionAsteroidplayer  p@(Player {playerlocation = (bx,by)}) a@( Asteroid {location = (ax,ay), status = s})
        | ax >= (bx-15) && ax <= (bx+15) && ay >= (by-15) && ay <= (by+15) = True
        | otherwise = False
    
    asteroidPlayer :: World -> World
    asteroidPlayer w@(World {asteroids = []}) = w
    asteroidPlayer w@(World {asteroids = listOfAsteroids, lives = l ,player = p}) 
        | all (==False) (map (collisionAsteroidplayer p) listOfAsteroids) == True = w 
        | otherwise = w{lives = (l -1)}
        
   

    --calcscore

    scoreChecker :: World -> World
    scoreChecker w@(World {asteroids = asteroidList, score = s}) = w{ score = length (filter (==Destroyed) (statelist asteroidList))}
                where 
                    statelist :: [Asteroid] -> [DestroyedOrNot]
                    statelist x = [ s |  (Asteroid { location = l, status = s}) <- x ]
     

    
