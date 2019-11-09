-- | This module defines how the state changes
--   in response to time and user input
module Controller where

import Model

import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import System.Random
import System.Environment
import Control.Monad (when)
import Data.Function (on)
import Data.List (sortBy)


-- | Handle one iteration of the game
step :: Float -> GameState -> IO GameState
step secs gstate  
    | (isdead $ player $ world gstate) == Dead && scoreSaved gstate == False = do 
        scoreToTXT $ gstate
        return $ gstate {infoToShow = ShowDeathscreen $ world gstate, world = world gstate, scoreSaved = True}
    | (isdead $ player $ world gstate) == Dead && scoreSaved gstate = return $ gstate {infoToShow = ShowDeathscreen $ world gstate, world = world gstate}
    | (isPaused $ world gstate) == True = return $ gstate {infoToShow = ShowWorld $ world gstate, world = world gstate}
    | otherwise = return $ gstate { elapsedTime = elapsedTime gstate + secs, infoToShow = ShowWorld(updateWorld $ world gstate), world = updateWorld $ world gstate}

-- | Handle user input
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- Key input

inputKey :: Event -> GameState -> GameState  
--laat staan tot nader order
{-
inputKey (EventKey (SpecialKey (KeyLeft)) Down _ _)  gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = UpleftMovement}}}
inputKey (EventKey (SpecialKey (KeyLeft)) Down _ _)  gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = DownleftMovement}}}
inputKey (EventKey (SpecialKey (KeyRight)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = UprightMovement}}}
inputKey (EventKey (SpecialKey (KeyRight)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = DownrightMovement}}}
-}
{-
inputKey (EventKey (SpecialKey (KeyLeft)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = LeftMovement}}}
inputKey (EventKey (SpecialKey (KeyRight)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = RightMovement}}}
inputKey (EventKey (SpecialKey (KeyUp)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = UpMovement}}}
inputKey (EventKey (SpecialKey (KeyDown)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = DownMovement}}}    
-}
inputKey (EventKey (Char ('w')) Down _ _) gstate@(GameState _ w@(World{player = p}) _ _) = gstate {world = w {player = p {movement = UpMovement}}}
inputKey (EventKey (Char ('a')) Down _ _) gstate@(GameState _ w@(World{player = p}) _ _) = gstate {world = w {player = p {movement = LeftMovement}}}
inputKey (EventKey (Char ('s')) Down _ _) gstate@(GameState _ w@(World{player = p}) _ _) = gstate {world = w {player = p {movement = DownMovement}}}
inputKey (EventKey (Char ('d')) Down _ _) gstate@(GameState _ w@(World{player = p}) _ _) = gstate {world = w {player = p {movement = RightMovement}}}
inputKey (EventKey (Char ('e')) Down _ _) gstate@(GameState _ w@(World{player = p}) _ _) = gstate {world = w {player = p {movement = UprightMovement}}}
inputKey (EventKey (Char ('q')) Down _ _) gstate@(GameState _ w@(World{player = p}) _ _) = gstate {world = w {player = p {movement = UpleftMovement}}}

--bullet
inputKey (EventKey (Char ('m')) _ _ _) gstate@(GameState _ w@(World {rockets = l}) _ _) = gstate {world = (spawnRocket w)}
inputKey (EventKey (SpecialKey (KeySpace)) _ _ _ ) gstate@(GameState _ w@(World {bullets = l}) _ _) = gstate {world = (spawnBullet w)}
inputKey (EventKey (Char ('p')) _ _ _) gstate@(GameState{world = w}) = gstate {world = w {pause = Paused} }
inputKey (EventKey (Char ('r')) _ _ _) gstate@(GameState{world = w}) = gstate {world = w {pause = Playing} }
inputKey _ gstate@(GameState _ w@(World{player = p}) _ _) = gstate {world = w {player = p {movement = NoMovement}}}

updateWorld :: World -> World
updateWorld w = 
    updatePlane $ 
    checklive $
    asteroidPlayer $
    planeOnScreen $ 
    updateRockets $
    updateBullets $
    scoreChecker $
    updateAsteroids $
    updateEnemies $ 
    enemyPlayer $
    enemyBulletPlayer $
    timeToSpawnEnemybullet $
    eos $
    asteroidBullet $
    bulletEnemy $
    enemyBullet $
    bulletAsteroid $
    asteroidRocket $
    rocketAsteroid $
    destroy_out_of_view_objects $
    removeDestroidObjects w

--stop game if dead
checklive :: World -> World
checklive w@(World {lives = l,player = p}) | l <= 0 = w{pause = Paused, player = (p{isdead = Dead})}
                                           | otherwise = w

-- Move plane
updatePlane :: World -> World

updatePlane w@(World {player = p@(Player {playerlocation = (x,y), movement = dir})})
    | dir == RightMovement = w{ player = p {playerlocation = (x + 11 ,y), movement = RightMovement}}
    | dir == LeftMovement = w{ player = p {playerlocation = (x - 11,y), movement = LeftMovement}}
    | dir == DownMovement = w{ player = p {playerlocation = (x,y - 11), movement = DownMovement}}
    | dir == UpMovement = w{ player = p {playerlocation = (x,y + 11), movement = UpMovement}}
    | dir == UpleftMovement = w{ player = p {playerlocation = (x - 5.5 , y + 5.5), movement = UpleftMovement}}
    | dir == UprightMovement = w{ player = p {playerlocation = (x + 5.5, y + 5.5), movement = UprightMovement}}
    | dir == NoMovement = w

-- Keep plane on screen
planeOnScreen :: World -> World
planeOnScreen w@(World {player = p@(Player {playerlocation = (x,y)})})
    | x <= (-200) = w{ player = p {playerlocation = (-200,y)}}
    | x >= (200) = w{ player = p {playerlocation = (200,y)}}
    | y <= (-190) = w{ player = p {playerlocation = (x,-190)}}
    | y >= (170) = w{ player = p {playerlocation = (x,170)}}
    | otherwise = w 

isPaused :: World -> Bool
isPaused w@(World {pause = paused})
    | paused == Playing = False
    | paused == Paused = True    

--epic object spawner
--hiermee kunnen we de moeilijkheid gaan controleren
--kunnen meer ettapes toevoegen
eos :: World -> World
eos w@(World {asteroidTimer = time, score = s}) | s < 20 = timeToSpawnAsteroid w
                                                | s >= 20  = timeToSpawnEnemy w

timeToSpawnEnemy :: World -> World
timeToSpawnEnemy w@(World {enemyTimer = time}) 
    | time < 1 = spawnEnemy w{ enemyTimer = 70}
    | otherwise = w {enemyTimer = time - 1}     

--enemies (experimental)
updateEnemies :: World -> World
updateEnemies w@(World {enemies = listOfEnemies}) = w{enemies = map updateEnemy listOfEnemies}

updateEnemy :: Enemy -> Enemy
updateEnemy e@(Enemy{ enemyLocation = (x,y), espeed = s}) = e{enemyLocation = (x,y+s)}

spawnEnemy :: World -> World
spawnEnemy w@(World {enemies = listOfEnemies,  asteroidsSpawnGenerator = g}) = w{enemies = listOfEnemies ++ [createEnemy (setRanNum g)], asteroidsSpawnGenerator = getGen (genereerRanNum g) }

createEnemy :: Float-> Enemy
createEnemy x = Enemy (x,200) NotDestroyed (-5)

-- rockets bitches

updateRockets :: World -> World
updateRockets w@(World {rockets = listOfRockets}) = w{rockets = map updateRocket listOfRockets}

updateRocket :: Rocket -> Rocket
updateRocket r@(Rocket{ rockLocation = (x,y), rspeed = s}) = r{ rockLocation = (x,(y+s)), rspeed = s}

spawnRocket :: World -> World
spawnRocket w@(World {player = p@(Player {playerlocation = (x,y)}), rockets = listOfRockets}) = w{rockets = listOfRockets ++ [(createRocket ((x-10),y))] ++ [(createRocket ((x+10),y))]  }

createRocket :: (Float,Float) -> Rocket
createRocket (x,y) = Rocket (x,y) 15 NotDestroyed

-- Update all asteroids    
updateAsteroids :: World -> World
updateAsteroids w@(World {asteroids = listOfAsteroids}) = w{asteroids = map updateAsteroid listOfAsteroids}

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid a@(Asteroid{ location = (x,y), aSpeed = s}) = a{ location = (x,y-s)}

spawnAsteroid :: World -> World
spawnAsteroid w@(World {asteroids = listOfAsteroids, asteroidsSpawnGenerator = g, oneThreeGenerator = otg, oneFiveGenerator = ofg }) = w{asteroids = listOfAsteroids ++ [createAsteroid (setRanNum g) (setRanNumOneThree g) 4 ],
asteroidsSpawnGenerator = getGen (genereerRanNum g),
oneThreeGenerator = getGen (genereerRanNumOneThree otg)}

createAsteroid :: Float -> Float -> Float -> Asteroid
createAsteroid x size speed = Asteroid (x,200) size NotDestroyed speed

timeToSpawnAsteroid :: World -> World
timeToSpawnAsteroid w@(World {asteroidTimer = time}) 
    | time < 1 = spawnAsteroid w{ asteroidTimer = 40}
    | otherwise = w {asteroidTimer = time - 1}

    -- random nummer generatie   
getGen :: (Float, StdGen) -> StdGen
getGen (_, g) = g

getRanNum :: (Float, StdGen) -> Float
getRanNum (x, _) = x

genereerRanNum :: StdGen -> (Float, StdGen)
genereerRanNum g = randomR ((-200), 200) g

setRanNum :: StdGen -> Float
setRanNum g = getRanNum (genereerRanNum g)

-- random nummer 1 - 3
genereerRanNumOneThree :: StdGen -> (Float, StdGen)
genereerRanNumOneThree g = randomR (1, 3) g

setRanNumOneThree :: StdGen -> Float
setRanNumOneThree g = getRanNum (genereerRanNumOneThree g)
-- random nummer 1 - 5

genereerRanNumOneFive :: StdGen -> (Float, StdGen)
genereerRanNumOneFive g = randomR (1, 5) g

setRanNumOneFive :: StdGen -> Float
setRanNumOneFive g = getRanNum (genereerRanNumOneFive g)

-- bullets

updateBullets :: World -> World
updateBullets w@(World {bullets = listOfBullets}) = w{bullets = map updateBullet listOfBullets}

updateBullet :: Bullet -> Bullet
updateBullet b@(Bullet{ bulletLocation = (x,y), speed = s}) = b{ bulletLocation = (x,(y+s)), speed = s}

spawnBullet :: World -> World
spawnBullet w@(World {player = p@(Player {playerlocation = (x,y)}), bullets = listOfBullets}) = w{bullets = listOfBullets ++ [(createBullet  Allied 10  (x,y))] }

spawnEnemyBullet :: Enemy -> Bullet
spawnEnemyBullet (Enemy {enemyLocation = (x,y)}) = (createBullet Notallied (-10) (x,y))

spawnEnemyBullets :: World -> World
spawnEnemyBullets w@(World {enemies = listofEnemies, bullets = listOfBullets}) = w{bullets = listOfBullets ++ (map spawnEnemyBullet listofEnemies)}

createBullet :: AlliedOrNot -> Float -> (Float,Float) -> Bullet
createBullet a s (x,y) = Bullet (x,y) s NotDestroyed a

timeToSpawnEnemybullet :: World -> World
timeToSpawnEnemybullet w@(World {schooterTimer = time}) 
    | time < 1 = spawnEnemyBullets w{ schooterTimer = 20}
    | otherwise = w {schooterTimer = time - 1}

--collision checkers
--asteroid and bullet

collisionAsteroidBullet :: Asteroid -> Bullet -> Bool
collisionAsteroidBullet a@(Asteroid {location = (ax,ay), status = s, size = si}) b@(Bullet {bulletLocation= (bx,by)})
    | ax >= (bx-si*7) && ax <= (bx+si*7) && ay >= (by-si*6) && ay <= (by+si*6) = True
    | otherwise = False

collisionBulletAsteroid :: Bullet -> Asteroid -> Bool
collisionBulletAsteroid b@(Bullet {bulletLocation= (bx,by)}) a@(Asteroid {location = (ax,ay), status = s, size = si})
    | ax >= (bx-si*7) && ax <= (bx+si*7) && ay >= (by-si*6) && ay <= (by+si*6) = True
    | otherwise = False    

asteroidBullet :: World -> World
asteroidBullet w@(World {asteroids = []}) = w
asteroidBullet w@(World {asteroids = listOfAsteroids, bullets = listOfBullets}) = w{asteroids = map check listOfAsteroids}
        where
            check asteroid | all (==False) (map (collisionAsteroidBullet asteroid) listOfBullets) == True = asteroid
                            | otherwise = asteroid{status = Destroyed}
                            
bulletAsteroid :: World -> World
bulletAsteroid w@(World {bullets = []}) = w
bulletAsteroid w@(World {asteroids = listOfAsteroids, bullets = listOfBullets}) = w{bullets = map check listOfBullets}
            where 
                check bullet | all (==False) (map (collisionBulletAsteroid bullet) listOfAsteroids) == True = bullet
                                | otherwise = bullet{bulletStatus = Destroyed}

--asteroid and rocket
collisionAsteroidRocket :: Asteroid -> Rocket -> Bool
collisionAsteroidRocket a@(Asteroid {location = (ax,ay), status = s, size = si}) b@(Rocket {rockLocation = (bx,by)}) 
    | ax >= (bx-si*7) && ax <= (bx+si*7) && ay >= (by-si*6) && ay <= (by+si*6) = True
    | otherwise = False

collisionRocketAsteroid :: Rocket -> Asteroid -> Bool
collisionRocketAsteroid b@(Rocket {rockLocation= (bx,by)}) a@(Asteroid {location = (ax,ay), status = s, size = si})
    | ax >= (bx-si*7) && ax <= (bx+si*7) && ay >= (by-si*6) && ay <= (by+si*6) = True
    | otherwise = False    

asteroidRocket :: World -> World
asteroidRocket w@(World {asteroids = []}) = w
asteroidRocket w@(World {asteroids = listOfAsteroids, rockets = listOfRockets}) = w{asteroids = map check listOfAsteroids}
        where
            check asteroid | all (==False) (map (collisionAsteroidRocket asteroid) listOfRockets) == True = asteroid
                            | otherwise = asteroid{status = Destroyed}
                            
rocketAsteroid :: World -> World
rocketAsteroid w@(World {rockets = []}) = w
rocketAsteroid w@(World {asteroids = listOfAsteroids, rockets = listOfRockets}) = w{rockets = map check listOfRockets}
            where 
                check rocket | all (==False) (map (collisionRocketAsteroid rocket) listOfAsteroids) == True = rocket
                                | otherwise = rocket{rocketStatus = Destroyed}

--player and asteroid        
-- collision Asteroid and player
-- hitboxen passen niet best bij player model nu!!
-- als player een asteroid op x = px+32 en y = py+ 32 heeft word het als een hit gezien maar het is natuurlijk niet echt een hit eg, de hitbox is een vierkant nu...        

collisionAsteroidPlayer :: Asteroid -> Player -> Bool
collisionAsteroidPlayer  a@( Asteroid {location = (ax,ay), size = si}) p@(Player {playerlocation = (px,py)})
    | ax >= (px-32) && ax <= (px+32) && ay >= (py-32) && ay <= (py+32) = True
    | otherwise = False    

asteroidPlayer :: World -> World
asteroidPlayer w@(World {asteroids = []}) = w
asteroidPlayer w@(World {asteroids = listOfAsteroids, player = p, lives = l}) = w{asteroids = map fst (map check listOfAsteroids), lives = minimum (map snd (map check listOfAsteroids))}
        where
            check asteroid | collisionAsteroidPlayer asteroid p  == False = (asteroid,l)
                            | otherwise = (asteroid{status = Destroyed},(l-1))



--player and enemy
collisionEnemyPlayer :: Enemy -> Player -> Bool
collisionEnemyPlayer  e@( Enemy {enemyLocation = (ax,ay)}) p@(Player {playerlocation = (px,py)}) 
    | ax >= (px-36) && ax <= (px+36) && ay >= (py-35) && ay <= (py+35) = True
    | otherwise = False  

enemyPlayer :: World -> World
enemyPlayer w@(World {enemies = []}) = w
enemyPlayer w@(World {enemies = listOfenemies, player = p, lives = l}) = w{enemies = map fst (map check listOfenemies), lives = minimum (map snd (map check listOfenemies))}
        where
            check enemy | collisionEnemyPlayer enemy p  == False = (enemy,l)
                        | otherwise = (enemy{estatus = Destroyed},(l-1))



--player and enemybullet

collisionBulletPlayer :: Bullet -> Player -> Bool
collisionBulletPlayer b@(Bullet {bulletLocation= (bx,by)}) a@(Player {playerlocation = (ax,ay)})
    | ax >= (bx-32) && ax <= (bx+32) && ay >= (by-32) && ay <= (by+32) = True
    | otherwise = False    
                            
enemyBulletPlayer :: World -> World
enemyBulletPlayer w@(World {bullets = []}) = w
enemyBulletPlayer w@(World {bullets = listOfbullets, player = p, lives = l}) = w{bullets = map fst (map check listOfbullets), lives = minimum (map snd (map check listOfbullets))}
        where
            check bullet@(Bullet {bulletallegiance = a}) | (not(collisionBulletPlayer bullet p)) || a == Allied  = (bullet,l)
                                                         | otherwise = (bullet{bulletStatus = Destroyed},(l-1))

--enemy and playerbullet

collisionEnemyBullet :: Enemy -> Bullet -> Bool
collisionEnemyBullet e@(Enemy {enemyLocation = (ax,ay), estatus = s}) b@(Bullet {bulletLocation= (bx,by), bulletallegiance = a})
    | ax >= (bx-36.5) && ax <= (bx+36.5) && ay >= (by-36.5) && ay <= (by+36.5) && a == Allied = True
    | otherwise = False


collisionBulletEnemy :: Bullet -> Enemy -> Bool
collisionBulletEnemy b@(Bullet {bulletLocation= (bx,by),bulletallegiance = a}) e@(Enemy {enemyLocation = (ax,ay)})
    | ax >= (bx-36) && ax <= (bx+36) && ay >= (by-36) && ay <= (by+36) && a == Allied = True
    | otherwise = False    

enemyBullet :: World -> World
enemyBullet w@(World {enemies = []}) = w
enemyBullet w@(World {enemies = listOfEnemies, bullets = listOfBullets}) = w{enemies = map check listOfEnemies}
        where
            check enemy | all (==False) (map (collisionEnemyBullet enemy) listOfBullets) == True = enemy
                        | otherwise = enemy{estatus = Destroyed}
                    
bulletEnemy :: World -> World
bulletEnemy w@(World {bullets = []}) = w
bulletEnemy w@(World {enemies = listOfEnemies, bullets = listOfBullets}) = w{bullets = map check listOfBullets}
            where 
                check bullet | all (==False) (map (collisionBulletEnemy bullet) listOfEnemies) == True = bullet
                                | otherwise = bullet{bulletStatus = Destroyed}

--calcscore
scoreChecker :: World -> World
scoreChecker w@(World {bullets = []}) = w
scoreChecker w@(World {asteroids = listOfAsteroids, bullets = listOfBullets, enemies = listOfEnemies, score = s}) = w{score =  maximum ((map(check1 s)listOfBullets) ++ (map (check s) listOfBullets))   }
            where 
                check sc bullet | all (==False) (map (collisionBulletAsteroid bullet) listOfAsteroids) == True = sc
                                | otherwise = (sc + 1)
                check1 sc bullet@(Bullet { bulletallegiance = a}) | (all (==False) (map (collisionBulletEnemy bullet) listOfEnemies) == True) = sc
                                                                  | otherwise = (sc + 1)
            

--destroy out of bounds

destroy_out_of_view_objects :: World -> World
destroy_out_of_view_objects w = w{ 
    bullets = (destroy_out_of_view_bullets w), 
    asteroids = (destroy_out_of_view_asteroids w) , 
    rockets = (destroy_out_of_view_rockets w),
    enemies = (destroy_out_of_view_enemies w)}
    where 
        destroy_out_of_view_bullets :: World -> [Bullet]
        destroy_out_of_view_bullets w@(World {bullets = []}) = []
        destroy_out_of_view_bullets w@(World {bullets = listOfBullets'}) = map check listOfBullets'
        check :: Bullet -> Bullet
        check b@(Bullet {bulletLocation = (_,y)}) | y > 200 || y < (-200) = b{bulletStatus = Destroyed}
                                                    | otherwise = b 
        destroy_out_of_view_asteroids :: World -> [Asteroid]
        destroy_out_of_view_asteroids w@(World {asteroids = []}) = []
        destroy_out_of_view_asteroids w@(World {asteroids = listOfAsteroids'}) = map check1 listOfAsteroids'
        check1 :: Asteroid -> Asteroid
        check1 a@(Asteroid {location = (_,y), size = si}) | y < (-200-6*si) = a{status = Destroyed}
                                                            | otherwise = a
        destroy_out_of_view_rockets :: World -> [Rocket]
        destroy_out_of_view_rockets w@(World {rockets = []}) = []
        destroy_out_of_view_rockets w@(World {rockets = listofRockets'}) = map check2 listofRockets'
        check2 :: Rocket-> Rocket
        check2 r@(Rocket{rockLocation = (_,y)}) | y > 200 = r{rocketStatus = Destroyed}
                                                | otherwise = r
        destroy_out_of_view_enemies :: World -> [Enemy]
        destroy_out_of_view_enemies  w@(World {enemies = []}) = []
        destroy_out_of_view_enemies w@(World {enemies = listOfEnemies'}) = map check3 listOfEnemies'
        check3 :: Enemy -> Enemy
        check3 e@(Enemy{enemyLocation = (_,y)}) | y < (-220) = e{estatus = Destroyed}
                                                 | otherwise = e
--remove destroyed stuff
removeDestroidObjects :: World -> World
removeDestroidObjects w@(World {asteroids = listOfAsteroids, bullets = listOfBullets, rockets = listofRockets, enemies = listofEnemies}) = w{
    asteroids = getOnlyNotDesAst listOfAsteroids,
    bullets = getOnlyNotDesBul listOfBullets,
    rockets = getOnlyNotDesRock listofRockets,
    enemies = getOnlyNotDesEnemy listofEnemies

    }
    where
        getOnlyNotDesAst :: [Asteroid] -> [Asteroid]
        getOnlyNotDesAst list = [a | a@(Asteroid {status = NotDestroyed}) <- list]
        getOnlyNotDesBul :: [Bullet] -> [Bullet]
        getOnlyNotDesBul list = [b | b@(Bullet {bulletStatus = NotDestroyed}) <- list]
        getOnlyNotDesRock :: [Rocket] -> [Rocket]
        getOnlyNotDesRock list = [c | c@(Rocket {rocketStatus = NotDestroyed}) <- list]
        getOnlyNotDesEnemy :: [Enemy] -> [Enemy]
        getOnlyNotDesEnemy list = [d | d@(Enemy {estatus = NotDestroyed}) <- list]



-- File handling
-- Write the current score to the "scores.txt" file
scoreToTXT :: GameState -> IO GameState
scoreToTXT gstate@(GameState{world = w@(World {score = p})})  = 
    do
        name <- getLine
        f <- readFile "scores.txt"
        let scores = f ++ ['\n'] ++ show p ++ name
        when (length scores > 0) $ writeFile "scores.txt" scores
        return gstate

--Read the highscores from the "scores.txt" file
getScoreFromTXT :: IO [String]
getScoreFromTXT = 
    do
        f <- readFile "scores.txt"
        if (length f > 1) then do
            let content =  lines f
                tups = map readTup content
                stups = take 3 (sortBy (flip compare `on` fst) tups) -- sorted list of highscores
                scores = map scoreString stups
            return (reverse scores)
        else return []

scoreString :: (Int, String) -> String
scoreString (x,y) = y ++ ": " ++ show x


readTup :: String -> (Int, String)
readTup scoreNameTuple = (score, name)
    where [(score, name)] = reads scoreNameTuple