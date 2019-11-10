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
    | playerStatus ( player $ world gstate) == Dead && not (scoreSaved gstate) = do 
        scoreToTXT gstate
        return $ gstate {infoToShow = ShowDeathscreen $ world gstate, world = world gstate, scoreSaved = True}
    | playerStatus ( player $ world gstate) == Dead && scoreSaved gstate = return $ gstate {infoToShow = ShowDeathscreen $ world gstate, world = world gstate}
    | (isPaused $ world gstate) = return $ gstate {infoToShow = ShowWorld $ world gstate, world = world gstate}
    | otherwise = return $ gstate {infoToShow = ShowWorld(updateWorld $ world gstate), world = updateWorld $ world gstate}

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
inputKey (EventKey (Char ('w')) Down _ _) gstate@(GameState _ w@(World{player = p})  _) = gstate {world = w {player = p {movement = UpMovement}}}
inputKey (EventKey (Char ('a')) Down _ _) gstate@(GameState _ w@(World{player = p})  _) = gstate {world = w {player = p {movement = LeftMovement}}}
inputKey (EventKey (Char ('s')) Down _ _) gstate@(GameState _ w@(World{player = p})  _) = gstate {world = w {player = p {movement = DownMovement}}}
inputKey (EventKey (Char ('d')) Down _ _) gstate@(GameState _ w@(World{player = p})  _) = gstate {world = w {player = p {movement = RightMovement}}}
inputKey (EventKey (Char ('e')) Down _ _) gstate@(GameState _ w@(World{player = p})  _) = gstate {world = w {player = p {movement = UprightMovement}}}
inputKey (EventKey (Char ('q')) Down _ _) gstate@(GameState _ w@(World{player = p})  _) = gstate {world = w {player = p {movement = UpleftMovement}}}

--bullet
inputKey (EventKey (SpecialKey (KeySpace)) _ _ _ ) gstate@(GameState _ w@(World {bullets = l})  _) = gstate {world = (spawnBullet w)}
inputKey (EventKey (Char ('p')) _ _ _) gstate@(GameState{world = w}) = gstate {world = w {pause = Paused} }
inputKey (EventKey (Char ('r')) _ _ _) gstate@(GameState{world = w}) = gstate {world = w {pause = Playing} }
inputKey _ gstate@(GameState _ w@(World{player = p})  _) = gstate {world = w {player = p {movement = NoMovement}}}

updateWorld :: World -> World
updateWorld w = 
    updatePlane $ 
    checklive $
    asteroidPlayer $
    planeOnScreen $ 
    updateBullets $
    scoreChecker $
    updateAsteroids $
    updateEnemies $ 
    enemyPlayer $
    enemyBulletPlayer $
    timeToSpawnEnemybullet $
    eos $
    keepAsteroidsOnScreen $
    asteroidBullet $
    enemyBullet $
    destroy_out_of_view_objects $
    moveRocks $
    removeDestroidObjects w

--stop game if dead
checklive :: World -> World
checklive w@(World {lives = l,player = p}) | l <= 0 = w{pause = Paused, player = (p{playerStatus= Dead})}
                                           | otherwise = w

-- Move plane
updatePlane :: World -> World

updatePlane w@(World {player = p@(Player {playerLocation = (x,y), movement = dir})})
    | dir == RightMovement = w{ player = p {playerLocation = (x + 10 ,y), movement = RightMovement}}
    | dir == LeftMovement = w{ player = p {playerLocation = (x - 10,y), movement = LeftMovement}}
    | dir == DownMovement = w{ player = p {playerLocation = (x,y - 10), movement = DownMovement}}
    | dir == UpMovement = w{ player = p {playerLocation = (x,y + 10), movement = UpMovement}}
    | dir == UpleftMovement = w{ player = p {playerLocation = (x - 5.5 , y + 5.5), movement = UpleftMovement}}
    | dir == UprightMovement = w{ player = p {playerLocation = (x + 5.5, y + 5.5), movement = UprightMovement}}
    | dir == NoMovement = w

-- Keep plane on screen
planeOnScreen :: World -> World
planeOnScreen w@(World {player = p@(Player {playerLocation = (x,y)})})
    | x <= (-200) = w{ player = p {playerLocation = (-200,y)}}
    | x >= 200 = w{ player = p {playerLocation = (200,y)}}
    | y <= (-190) = w{ player = p {playerLocation = (x,-190)}}
    | y >= 170 = w{ player = p {playerLocation = (x,170)}}
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
                                                | s >= 20 && s <  40 = timeToSpawnEnemy False $ timeToSpawnAsteroid w
                                                | s >= 40 = timeToSpawnEnemy True $ timeToSpawnAsteroid w


timeToSpawnEnemy :: Bool -> World -> World
timeToSpawnEnemy f w@(World {enemyTimer = time}) 
    | f==False && time < 1 = spawnRandomEnemy w{ enemyTimer = 70}
    | time < 1 = spawnAttackEnemy w{ enemyTimer = 70}
    | otherwise = w {enemyTimer = time - 1}     

--enemies (experimental)
updateEnemies :: World -> World
updateEnemies w@(World {enemies = listOfEnemies}) = w{enemies = map updateEnemy listOfEnemies}

updateEnemy :: Enemy -> Enemy
updateEnemy e@(Enemy{ enemyLocation = (x,y), enemySpeed = s}) = e{enemyLocation = (x,y+s)}

spawnRandomEnemy :: World -> World
spawnRandomEnemy w@(World {enemies = listOfEnemies,  asteroidsSpawnGenerator = g}) = w{enemies = listOfEnemies ++ [createEnemy (getFirstNumber(generateTreeNumbers g)) ], asteroidsSpawnGenerator = getSeed(generateTreeNumbers g) }

spawnAttackEnemy :: World -> World
spawnAttackEnemy w@(World {enemies = listOfEnemies, player = p@(Player {playerLocation = (x,y)})}) = w{enemies = listOfEnemies ++ [createEnemy x ]}


createEnemy :: Float-> Enemy
createEnemy x = Enemy (x,200) NotDestroyed (-3)

-- Update all asteroids    
updateAsteroids :: World -> World
updateAsteroids w@(World {asteroids = listOfAsteroids}) = w{asteroids = map updateAsteroid listOfAsteroids}

updateAsteroid :: Asteroid -> Asteroid
updateAsteroid a@(Asteroid{ asteroidLocation = al, asteroidSpeed = s, asteroidHeading = ah}) = a{ asteroidLocation = translatePointVector al ah}

spawnAsteroid :: World -> World
spawnAsteroid w@(World {asteroids = listOfAsteroids, asteroidsSpawnGenerator = g}) = w{asteroids = listOfAsteroids ++ [createAsteroid (getFirstNumber(generateTreeNumbers g)) (getSecondNumber(generateTreeNumbers g)) (getThirdNumber(generateTreeNumbers g)) (getVectorAsteroid(generateVectorAsteroid g))]
, asteroidsSpawnGenerator = getSeed(generateTreeNumbers g)}

createAsteroid :: Float -> Float -> Float -> Vector -> Asteroid
createAsteroid x asteroidSize = Asteroid (x, 200) asteroidSize NotDestroyed

keepAsteroidsOnScreen :: World -> World
keepAsteroidsOnScreen w@(World {asteroids = listOfAsteroids}) = w{asteroids = map onScreen listOfAsteroids}
    where 
        onScreen a@(Asteroid {asteroidLocation = (ax,_),asteroidHeading = (hx,hy)}) | ax < -240 = a{asteroidHeading = (hx * (-1),hy)}
                                                                                    | ax > 240 = a{asteroidHeading = (hx * (-1),hy)}
                                                                                    | otherwise = a
-- Update all asteroids    
moveRocks :: World -> World
moveRocks w@(World {rocks = listOfRocks}) = w{rocks = map updateRock (checkRockDestryed listOfRocks)}

updateRock :: Rock -> Rock
updateRock r@(Rock{ rockLocation = rl, rockHeading = rh, liveTime = lt, rockStatus = rs}) 
    |  lt < 0 = r{ rockStatus = Destroyed}
    | otherwise = r{ rockLocation = translatePointVector rl rh, liveTime = lt -1}

checkRockDestryed :: [Rock] -> [Rock]
checkRockDestryed rockList = [r | r@(Rock {rockStatus = NotDestroyed}) <- rockList]

-- Animation / explosion

spawnCluster :: Point -> StdGen -> [Rock]
spawnCluster p g = [createRock p (getFirstVector (generaterVectorCluster g))] ++ 
                    [createRock p (getSecondVector (generaterVectorCluster g))] ++ 
                    [createRock p (getThirdVector (generaterVectorCluster g))] ++ 
                    [createRock p (getFourthVector (generaterVectorCluster g))] ++
                    [createRock p (getFifthVector (generaterVectorCluster g))]

createRock :: Point -> Vector -> Rock
createRock p v = Rock p v 4 NotDestroyed


timeToSpawnAsteroid :: World -> World
timeToSpawnAsteroid w@(World {asteroidTimer = time}) 
    | time < 1 = spawnAsteroid w{ asteroidTimer = 40}
    | otherwise = w {asteroidTimer = time - 1}

translatePointVector :: Point -> Vector -> Point
translatePointVector (x1,y1) (x2,y2) = (x1+x2,y1+y2)

generateTreeNumbers :: RandomGen g => g -> ((Float, Float, Float), g)
generateTreeNumbers g = let (v1, g1) = randomR (-200, 200) g
                            (v2, g2) = randomR (1, 5) g1 -- Use new seed
                            (v3, g3) = randomR (1, 20) g2 -- Use new seed
                       in ((v1, v2,v3), g3) -- Return last seed

getFirstNumber :: RandomGen g => ((Float, Float,Float), g) -> Float
getFirstNumber ((f, _,_), _) = f

getSecondNumber :: RandomGen g => ((Float, Float,Float), g) -> Float
getSecondNumber ((_, f,_), _) = f

getThirdNumber :: RandomGen g => ((Float, Float,Float), g) -> Float
getThirdNumber ((_, _,f), _) = f

getSeed :: RandomGen g => ((Float, Float,Float), g) -> g
getSeed ((_, _,_), g) = g

generateVectorAsteroid :: RandomGen g => g -> ((Float, Float), g)
generateVectorAsteroid g = let (v1, g1) = randomR (-4, 4) g
                               (v2, g2) = randomR (-1, -5) g1 -- Use new seed
                           in  ((v1, v2), g2) -- Return last seed

getVectorAsteroid :: RandomGen g => ((Float, Float), g) -> (Float, Float)
getVectorAsteroid ((x, y), _) = (x,y)

generaterVectorCluster :: RandomGen g => g -> ((Vector,Vector,Vector,Vector,Vector), g)
generaterVectorCluster g = let (x1, g1) = randomR (-4, 4) g
                               (y1, g2) = randomR (-4, 4) g1 -- Use new seed
                               (x2, g3) = randomR (-4, 4) g2
                               (y2, g4) = randomR (-4, 4) g3 -- Use new seed
                               (x3, g5) = randomR (-4, 4) g4
                               (y3, g6) = randomR (-4, 4) g5 -- Use new seed
                               (x4, g7) = randomR (-4, 4) g6
                               (y4, g8) = randomR (-4, 4) g7 -- Use new seed
                               (x5, g9) = randomR (-4, 4) g8
                               (y5, g10) = randomR (-4, 4) g9 -- Use new seed
                            in (((x1,y1),(x2,y2),(x3,y3),(x4,y4),(x5,y5)),g10)

getFirstVector :: RandomGen g => ((Vector,Vector,Vector,Vector,Vector), g) -> Vector
getFirstVector (((x,y),_,_,_,_), _) = (x,y)
getSecondVector :: RandomGen g => ((Vector,Vector,Vector,Vector,Vector), g) -> Vector
getSecondVector ((_,v,_,_,_), _) = v
getThirdVector :: RandomGen g => ((Vector,Vector,Vector,Vector,Vector), g) -> Vector
getThirdVector ((_,_,v,_,_), _) = v
getFourthVector :: RandomGen g => ((Vector,Vector,Vector,Vector,Vector), g) -> Vector
getFourthVector ((_,_,_,v,_), _) = v
getFifthVector :: RandomGen g => ((Vector,Vector,Vector,Vector,Vector), g) -> Vector
getFifthVector ((_,_,_,_,v), _) = v

-- bullets
updateBullets :: World -> World
updateBullets w@(World {bullets = listOfBullets}) = w{bullets = map updateBullet listOfBullets}

updateBullet :: Bullet -> Bullet
updateBullet b@(Bullet{ bulletLocation = (x,y), bulletSpeed = s}) = b{ bulletLocation = (x,y+s), bulletSpeed = s}

spawnBullet :: World -> World
spawnBullet w@(World {player = p@(Player {playerLocation = (x,y)}), bullets = listOfBullets}) = w{bullets = listOfBullets ++ [createBullet  Allied 10  (x,y)] }

spawnEnemyBullet :: Enemy -> Bullet
spawnEnemyBullet (Enemy {enemyLocation = (x,y)}) = createBullet Notallied (-10) (x,y)

spawnEnemyBullets :: World -> World
spawnEnemyBullets w@(World {enemies = listofEnemies, bullets = listOfBullets}) = w{bullets = listOfBullets ++ map spawnEnemyBullet listofEnemies}

createBullet :: AlliedOrNot -> Float -> Point -> Bullet
createBullet a s (x,y) = Bullet (x,y) s NotDestroyed a

timeToSpawnEnemybullet :: World -> World
timeToSpawnEnemybullet w@(World {schooterTimer = time}) 
    | time < 1 = spawnEnemyBullets w{ schooterTimer = 20}
    | otherwise = w {schooterTimer = time - 1}

--collision checkers
--asteroid and bullet

collisionAsteroidBullet :: Asteroid -> Bullet -> Bool
collisionAsteroidBullet Asteroid {asteroidLocation = (ax,ay), asteroidStatus = s, asteroidSize = si} Bullet {bulletLocation= (bx,by)}
    | ax >= (bx-si*7) && ax <= (bx+si*7) && ay >= (by-si*6) && ay <= (by+si*6) = True
    | otherwise = False

asteroidBullet :: World -> World
asteroidBullet w@(World {asteroids = []}) = w
asteroidBullet w@(World {rocks = listOfRocks, asteroidsSpawnGenerator = g, asteroids = listOfAsteroids, bullets = listOfBullets}) = w{asteroids = map check listOfAsteroids, bullets = map check2 listOfBullets, rocks = listOfRocks ++ concatMap check3 listOfBullets}
    where
        check asteroid  | all (==False) (map (collisionAsteroidBullet asteroid) listOfBullets)  = asteroid
                        | otherwise = asteroid{asteroidStatus = Destroyed}
        check2 bullet   | all (==False) (map (flip collisionAsteroidBullet bullet) listOfAsteroids)  = bullet
                        | otherwise = bullet{bulletStatus = Destroyed}
        check3 b@(Bullet{bulletLocation = (bx,by)})   | all (==False) (map (flip collisionAsteroidBullet b) listOfAsteroids) == True = []
                        | otherwise = spawnCluster (bx,by) g

-- collision Asteroid and player
-- hitboxen passen niet best bij player model nu!!
-- als player een asteroid op x = px+32 en y = py+ 32 heeft word het als een hit gezien maar het is natuurlijk niet echt een hit eg, de hitbox is een vierkant nu...        

collisionAsteroidPlayer :: Asteroid -> Player -> Bool
collisionAsteroidPlayer  a@( Asteroid {asteroidLocation = (ax,ay), asteroidSize = si}) p@(Player {playerLocation = (px,py)})
    | ax >= (px-32) && ax <= (px+32) && ay >= (py-32) && ay <= (py+32) = True
    | otherwise = False    

asteroidPlayer :: World -> World
asteroidPlayer w@(World {asteroids = []}) = w
asteroidPlayer w@(World {asteroids = listOfAsteroids, player = p, lives = l}) = w{asteroids = map (fst.check) listOfAsteroids, lives = minimum (map (snd.check) listOfAsteroids)}
    where
        check asteroid | not (collisionAsteroidPlayer asteroid p) = (asteroid,l)
                       | otherwise = (asteroid{asteroidStatus = Destroyed},l-1)

--player and enemy
collisionEnemyPlayer :: Enemy -> Player -> Bool
collisionEnemyPlayer  e@( Enemy {enemyLocation = (ax,ay)}) p@(Player {playerLocation = (px,py)}) 
    | ax >= (px-36) && ax <= (px+36) && ay >= (py-35) && ay <= (py+35) = True
    | otherwise = False  

enemyPlayer :: World -> World
enemyPlayer w@(World {enemies = []}) = w
enemyPlayer w@(World {enemies = listOfenemies, player = p, lives = l}) = w{enemies = map (fst.check) listOfenemies, lives = minimum (map (snd.check) listOfenemies)}
    where
        check enemy | not (collisionEnemyPlayer enemy p)  = (enemy,l)
                    | otherwise = (enemy{enemyStatus = Destroyed},l-1)

--player and enemybullet
collisionBulletPlayer :: Bullet -> Player -> Bool
collisionBulletPlayer b@(Bullet {bulletLocation= (bx,by)}) a@(Player {playerLocation = (ax,ay)})
    | ax >= (bx-32) && ax <= (bx+32) && ay >= (by-32) && ay <= (by+32) = True
    | otherwise = False    
                            
enemyBulletPlayer :: World -> World
enemyBulletPlayer w@(World {bullets = []}) = w
enemyBulletPlayer w@(World {bullets = listOfbullets, player = p, lives = l}) = w{bullets = map (fst.check) listOfbullets, lives = minimum (map (snd.check) listOfbullets)}
    where
        check bullet@(Bullet {bulletAllegiance = a}) | not(collisionBulletPlayer bullet p) || a == Allied  = (bullet,l)
                                                    | otherwise = (bullet{bulletStatus = Destroyed},(l-1))

--enemy and playerbullet
collisionEnemyBullet :: Enemy -> Bullet -> Bool
collisionEnemyBullet e@(Enemy {enemyLocation = (ax,ay), enemyStatus = s}) b@(Bullet {bulletLocation= (bx,by), bulletAllegiance = a})
    | ax >= (bx-36.5) && ax <= (bx+36.5) && ay >= (by-36.5) && ay <= (by+36.5) && a == Allied = True
    | otherwise = False
 
enemyBullet :: World -> World
enemyBullet w@(World {enemies = []}) = w
enemyBullet w@(World {enemies = listOfEnemies, bullets = listOfBullets}) = w{enemies = map check listOfEnemies,bullets = map check1 listOfBullets}
    where
        check enemy | all (==False) (map (collisionEnemyBullet enemy) listOfBullets) = enemy
                    | otherwise = enemy{enemyStatus = Destroyed}
        check1 bullet | all (==False) (map (flip collisionEnemyBullet bullet) listOfEnemies) = bullet
                      | otherwise = bullet{bulletStatus = Destroyed}

--calcscore
scoreChecker :: World -> World
scoreChecker w@(World {bullets = []}) = w
scoreChecker w@(World {asteroids = listOfAsteroids, bullets = listOfBullets, enemies = listOfEnemies, score = s}) = w{score =  maximum ((map(check1 s)listOfBullets) ++ (map (check s) listOfBullets)) }
    where 
        check sc bullet | all (==False) (map (flip collisionAsteroidBullet bullet) listOfAsteroids) == True = sc
                        | otherwise = (sc + 1)
        check1 sc bullet@(Bullet { bulletAllegiance = a}) | (all (==False) (map (flip collisionEnemyBullet bullet) listOfEnemies) == True) = sc
                                                          | otherwise = (sc + 1)
       
--destroy out of bounds

destroy_out_of_view_objects :: World -> World
destroy_out_of_view_objects w@(World{
    bullets = listOfBullets, 
    asteroids = listOfAsteroids, 
    enemies = listofEnemies}) = w{ 
    bullets = (map destroy_out_of_view_bullets listOfBullets), 
    asteroids = (map destroy_out_of_view_asteroids listOfAsteroids) , 
    enemies = (map destroy_out_of_view_enemies listofEnemies)}
    where 
        destroy_out_of_view_bullets :: Bullet -> Bullet
        destroy_out_of_view_bullets b@Bullet {bulletLocation = (_,y)} | y > 200 || y < (-200) = b{bulletStatus = Destroyed}
                                                                        | otherwise = b 
        destroy_out_of_view_asteroids :: Asteroid -> Asteroid
        destroy_out_of_view_asteroids a@Asteroid {asteroidLocation = (_,y), asteroidSize = si} | y < (-200-6*si) = a{asteroidStatus = Destroyed}
                                                                                                | otherwise = a
        destroy_out_of_view_enemies :: Enemy -> Enemy
        destroy_out_of_view_enemies e@Enemy{enemyLocation = (_,y)} | y < (-220) = e{enemyStatus = Destroyed}
                                                 | otherwise = e
--remove destroyed stuff
removeDestroidObjects :: World -> World
removeDestroidObjects w@World {asteroids = listOfAsteroids, bullets = listOfBullets, enemies = listofEnemies} = w{
    asteroids = getOnlyNotDesAst listOfAsteroids,
    bullets = getOnlyNotDesBul listOfBullets,
    enemies = getOnlyNotDesEnemy listofEnemies
    }
    where
        getOnlyNotDesAst :: [Asteroid] -> [Asteroid]
        getOnlyNotDesAst list = [a | a@Asteroid {asteroidStatus = NotDestroyed} <- list]
        getOnlyNotDesBul :: [Bullet] -> [Bullet]
        getOnlyNotDesBul list = [b | b@Bullet {bulletStatus = NotDestroyed} <- list]
        getOnlyNotDesEnemy :: [Enemy] -> [Enemy]
        getOnlyNotDesEnemy list = [e | e@Enemy {enemyStatus = NotDestroyed} <- list]
    
-- File handling
-- Write the current score to the "scores.txt" file
scoreToTXT :: GameState -> IO GameState
scoreToTXT gstate@(GameState{world = w@(World {score = p})})  = 
    do
        name <- getLine
        f <- readFile "scores.txt"
        let scores = f ++ ['\n'] ++ show p ++ name
        when (not (null scores)) $ writeFile "scores.txt" scores
        return gstate

getScoreFromTXT :: IO [String]
getScoreFromTXT = 
    do
        f <- readFile "scores.txt"
        getsrc f 
    where
        getsrc c | length c > 1 = do
                                let content =  lines c
                                    tups = map readTup content
                                    stups = take 3 (sortBy (flip compare `on` fst) tups) -- sorted list of highscores
                                    scores = map scoreString stups
                                return (reverse scores)
                | otherwise = return []
        

scoreString :: (Int, String) -> String
scoreString (x,y) = y ++ ": " ++ show x

readTup :: String -> (Int, String)
readTup scoreNameTuple = (score, name)
    where [(score, name)] = reads scoreNameTuple

