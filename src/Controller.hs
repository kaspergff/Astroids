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


-- | Verwerk 1 interatie van de game
-- Als de player dood is en de naam nogniet is ingevuld laat dan de naam invullen en laat het eind scherm zien. Er is daarna een extra check die er voor zorgt dat de player maar 1x zijn naam kan invullen
-- één naar laatste verwerd pauze en herstarten
-- laaste ubdate de game elke stap
step :: Float -> GameState -> IO GameState
step secs gstate  
    | playerStatus ( player $ world gstate) == Dead && not (scoreSaved gstate) = do 
        scoreToTXT gstate
        return $ gstate {infoToShow = ShowDeathscreen $ world gstate, world = world gstate, scoreSaved = True}
    | playerStatus ( player $ world gstate) == Dead && scoreSaved gstate = return $ gstate {infoToShow = ShowDeathscreen $ world gstate, world = world gstate}
    | (isPaused $ world gstate) = return $ gstate {infoToShow = ShowWorld $ world gstate, world = world gstate}
    | otherwise = return $ gstate {infoToShow = ShowWorld(updateWorld $ world gstate), world = updateWorld $ world gstate}

-- | Verwerk input van de user
input :: Event -> GameState -> IO GameState
input e gstate = return (inputKey e gstate)

-- Key input
inputKey :: Event -> GameState -> GameState  

inputKey (EventKey (SpecialKey (KeyLeft)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = LeftMovement}}}
inputKey (EventKey (SpecialKey (KeyRight)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = RightMovement}}}
inputKey (EventKey (SpecialKey (KeyUp)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = UpMovement}}}
inputKey (EventKey (SpecialKey (KeyDown)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = DownMovement}}}    

--bullet
inputKey (EventKey (SpecialKey (KeySpace)) _ _ _ ) gstate@(GameState _ w@(World {bullets = l})  _) = gstate {world = (spawnBullet w)}
inputKey (EventKey (Char ('p')) _ _ _) gstate@(GameState{world = w}) = gstate {world = w {pause = Paused} }
inputKey (EventKey (Char ('r')) _ _ _) gstate@(GameState{world = w}) = gstate {world = w {pause = Playing} }
inputKey _ gstate@(GameState _ w@(World{player = p})  _) = gstate {world = w {player = p {movement = NoMovement}}}

-- main update functie de alles in de wereld update
updateWorld :: World -> World
updateWorld w = 
    checklive $ -- leeft player nog
    planeOnScreen $  -- zorg dat player niet van het scherm kan
    updatePlane $  -- verplaats player
    updateBullets $ -- verplaatst bullets
    updateAsteroids $ -- verplaats asteroids
    updateEnemies $ -- verplaats enemies
    moveRocks $ -- verplaatst rocks
    -- collision
    asteroidPlayer $ 
    enemyPlayer $
    enemyBulletPlayer $
    asteroidBullet $
    enemyBullet $

    scoreChecker $ -- check score
    timeToSpawnEnemybullet $ -- nieuw schot enemies
    eos $ -- welke enemies worden gespawned
    keepAsteroidsOnScreen $ -- zorg dat asteroids op scherm blijven
    destroy_out_of_view_objects $ -- destroy objects ver buiten het scherm
    removeDestroidObjects w -- verwijder destroyed objecten

--Controleer of de player nog levens heeft. Als de player geen levens heeft stop de game
checklive :: World -> World
checklive w@(World {lives = l,player = p}) | l <= 0 = w{pause = Paused, player = (p{playerStatus= Dead})}
                                           | otherwise = w

-- Move plane over screen
updatePlane :: World -> World
updatePlane w@(World {player = p@(Player {playerLocation = (x,y), movement = dir})})
    | dir == RightMovement = w{ player = p {playerLocation = (x + 10 ,y), movement = RightMovement}}
    | dir == LeftMovement = w{ player = p {playerLocation = (x - 10,y), movement = LeftMovement}}
    | dir == DownMovement = w{ player = p {playerLocation = (x,y - 10), movement = DownMovement}}
    | dir == UpMovement = w{ player = p {playerLocation = (x,y + 10), movement = UpMovement}}
    | dir == NoMovement = w

-- Keep plane on screen
planeOnScreen :: World -> World
planeOnScreen w@(World {player = p@(Player {playerLocation = (x,y)})})
    | x <= (-168) = w{ player = p {playerLocation = (-168,y)}}
    | x >= 168 = w{ player = p {playerLocation = (168,y)}}
    | y <= (-168) = w{ player = p {playerLocation = (x,-168)}}
    | y >= 170 = w{ player = p {playerLocation = (x,170)}}
    | otherwise = w 
-- pauzeer de game
isPaused :: World -> Bool
isPaused w@(World {pause = paused})
    | paused == Playing = False
    | paused == Paused = True    

--epic object spawner
--hiermee kunnen we de moeilijkheid gaan controleren
--kunnen meer ettapes toevoegen
eos :: World -> World
eos w@(World {asteroidTimer = time, score = s}) | s < 20 = timeToSpawnAsteroid w -- alleen asteroids
                                                | s >= 20 && s <  40 = timeToSpawnEnemy False $ timeToSpawnAsteroid w --asteroids en random enemies
                                                | s >= 40 = timeToSpawnEnemy True $ timeToSpawnAsteroid w -- asteroids en attack enemies (spawnen boven player)

-- spawn enemie als de score boven de 40 is wordt de bool true en worden de enemies moeilijker
timeToSpawnEnemy :: Bool -> World -> World
timeToSpawnEnemy f w@(World {enemyTimer = time}) 
    | f==False && time < 1 = spawnRandomEnemy w{ enemyTimer = 70}
    | time < 1 = spawnAttackEnemy w{ enemyTimer = 70}
    | otherwise = w {enemyTimer = time - 1}     

--update alle enemies
updateEnemies :: World -> World
updateEnemies w@(World {enemies = listOfEnemies}) = w{enemies = map updateEnemy listOfEnemies}

-- update een enkele enemie
updateEnemy :: Enemy -> Enemy
updateEnemy e@(Enemy{ enemyLocation = (x,y), enemySpeed = s}) = e{enemyLocation = (x,y+s)}
-- spawn een random enemie
spawnRandomEnemy :: World -> World
spawnRandomEnemy w@(World {enemies = listOfEnemies,  asteroidsSpawnGenerator = g}) = w{enemies = listOfEnemies ++ [createEnemy (getFirstNumber(generateTreeNumbers g)) ], asteroidsSpawnGenerator = getSeed(generateTreeNumbers g) }
-- spawn een attack enemie (spanwned boven de player)
spawnAttackEnemy :: World -> World
spawnAttackEnemy w@(World {enemies = listOfEnemies, player = p@(Player {playerLocation = (x,y)})}) = w{enemies = listOfEnemies ++ [createEnemy x ]}

-- creer een enemie
createEnemy :: Float-> Enemy
createEnemy x = Enemy (x,200) NotDestroyed (-3)

-- Update all asteroids    
updateAsteroids :: World -> World
updateAsteroids w@(World {asteroids = listOfAsteroids}) = w{asteroids = map updateAsteroid listOfAsteroids}
-- update een enkele asteroid
updateAsteroid :: Asteroid -> Asteroid
updateAsteroid a@(Asteroid{ asteroidLocation = al, asteroidHeading = ah}) = a{ asteroidLocation = translatePointVector al ah}

-- spawn een asteroid
spawnAsteroid :: World -> World
spawnAsteroid w@(World {asteroids = listOfAsteroids, asteroidsSpawnGenerator = g}) = w{asteroids = listOfAsteroids ++ [createAsteroid (getFirstNumber(generateTreeNumbers g)) (getSecondNumber(generateTreeNumbers g)) (getVectorAsteroid(generateVectorAsteroid g))]
, asteroidsSpawnGenerator = getSeed(generateTreeNumbers g)}

-- creeer een asteroid
createAsteroid :: Float -> Float -> Vector -> Asteroid
createAsteroid x asteroidSize = Asteroid (x, 200) asteroidSize NotDestroyed
-- zorg dat de asteroids op het scherm blijven
keepAsteroidsOnScreen :: World -> World
keepAsteroidsOnScreen w@(World {asteroids = listOfAsteroids}) = w{asteroids = map onScreen listOfAsteroids}
    where 
        onScreen a@(Asteroid {asteroidLocation = (ax,_),asteroidHeading = (hx,hy)}) | ax < -240 = a{asteroidHeading = (hx * (-1),hy)}
                                                                                    | ax > 240 = a{asteroidHeading = (hx * (-1),hy)}
                                                                                    | otherwise = a
-- Update all rocks    
moveRocks :: World -> World
moveRocks w@(World {rocks = listOfRocks}) = w{rocks = map updateRock listOfRocks}
-- update een enkel rock als de liveTime van een rock lager dan 0 is wordt die destroyed
updateRock :: Rock -> Rock
updateRock r@(Rock{ rockLocation = rl, rockHeading = rh, liveTime = lt, rockStatus = rs}) 
    |  lt < 0 = r{ rockStatus = Destroyed}
    | otherwise = r{ rockLocation = translatePointVector rl rh, liveTime = lt -1}

-- spawn een cluster van rocks voor animeren explosie
spawnClusterFromAsteroid :: Point -> StdGen -> [Rock]
spawnClusterFromAsteroid p g = [createRock p (getFirstVector (generaterVectorCluster g)) FromAsteroid] ++ 
                    [createRock p (getSecondVector (generaterVectorCluster g))FromAsteroid] ++ 
                    [createRock p (getThirdVector (generaterVectorCluster g))FromAsteroid] ++ 
                    [createRock p (getFourthVector (generaterVectorCluster g))FromAsteroid] ++
                    [createRock p (getFifthVector (generaterVectorCluster g))FromAsteroid]

spawnClusterFromShip :: Point -> StdGen -> [Rock]
spawnClusterFromShip p g = [createRock p (getFirstVector (generaterVectorCluster g)) FromShip] ++ 
                    [createRock p (getSecondVector (generaterVectorCluster g))FromShip] ++ 
                    [createRock p (getThirdVector (generaterVectorCluster g))FromShip] ++ 
                    [createRock p (getFourthVector (generaterVectorCluster g))FromShip] ++
                    [createRock p (getFifthVector (generaterVectorCluster g))FromShip]                    
-- maak een enkel rock
createRock :: Point -> Vector -> AsteroidOrShip -> Rock
createRock p v aOs = Rock p v 4 NotDestroyed aOs

-- kijk of het tijd is om een asteriod te spawnen
timeToSpawnAsteroid :: World -> World
timeToSpawnAsteroid w@(World {asteroidTimer = time, score = score}) 
    | time < 1 && score < 15    = spawnAsteroid w{ asteroidTimer = 40}
    | time < 1 && score < 30    = spawnAsteroid w{ asteroidTimer = 30}
    | time < 1                  = spawnAsteroid w{ asteroidTimer = 20}
    | otherwise                 = w {asteroidTimer = time - 1}
-- move point acording to a vector
translatePointVector :: Point -> Vector -> Point
translatePointVector (x1,y1) (x2,y2) = (x1+x2,y1+y2)


-- bullets
-- update all bullets
updateBullets :: World -> World
updateBullets w@(World {bullets = listOfBullets}) = w{bullets = map updateBullet listOfBullets}
-- update single bullet
updateBullet :: Bullet -> Bullet
updateBullet b@(Bullet{ bulletLocation = (x,y), bulletSpeed = s}) = b{ bulletLocation = (x,y+s), bulletSpeed = s}
-- spawn bullet
spawnBullet :: World -> World
spawnBullet w@(World {player = p@(Player {playerLocation = (x,y)}), bullets = listOfBullets}) = w{bullets = listOfBullets ++ [createBullet  Allied 10  (x,y)] }
-- spawn one bullet
spawnEnemyBullet :: Enemy -> Bullet
spawnEnemyBullet (Enemy {enemyLocation = (x,y)}) = createBullet Notallied (-10) (x,y)
-- spawns all enemie bullets
spawnEnemyBullets :: World -> World
spawnEnemyBullets w@(World {enemies = listofEnemies, bullets = listOfBullets}) = w{bullets = listOfBullets ++ map spawnEnemyBullet listofEnemies}
-- creat a bullet
createBullet :: AlliedOrNot -> Float -> Point -> Bullet
createBullet a s (x,y) = Bullet (x,y) s NotDestroyed a
-- timer for enemies to shoot
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

-- checks collision asteroids and bullets. If there is a collision Destroy asteroid and bullet en spawn cluster of rocks    
asteroidBullet :: World -> World
asteroidBullet w@(World {asteroids = []}) = w
asteroidBullet w@(World {rocks = listOfRocks, asteroidsSpawnGenerator = g, asteroids = listOfAsteroids, bullets = listOfBullets}) = w{asteroids = map check listOfAsteroids, bullets = map check2 listOfBullets, rocks = listOfRocks ++ concatMap check3 listOfBullets}
    where
        check asteroid  | all (==False) (map (collisionAsteroidBullet asteroid) listOfBullets)  = asteroid
                        | otherwise = asteroid{asteroidStatus = Destroyed}
        check2 bullet   | all (==False) (map (flip collisionAsteroidBullet bullet) listOfAsteroids)  = bullet
                        | otherwise = bullet{bulletStatus = Destroyed}
        check3 b@(Bullet{bulletLocation = (bx,by)})   
                        | all (==False) (map (flip collisionAsteroidBullet b) listOfAsteroids) == True = []
                        | otherwise = spawnClusterFromAsteroid (bx,by) g

-- collision Asteroid and player
-- als player een asteroid op x = px+32 en y = py+ 32 heeft word het als een hit gezien maar het is natuurlijk niet echt een hit eg, de hitbox is een vierkant nu...        
-- collision asteroid en player
collisionAsteroidPlayer :: Asteroid -> Player -> Bool
collisionAsteroidPlayer  a@( Asteroid {asteroidLocation = (ax,ay), asteroidSize = si}) p@(Player {playerLocation = (px,py)})
    | ax >= (px-32) && ax <= (px+32) && ay >= (py-32) && ay <= (py+32) = True
    | otherwise = False    

-- destroy asteroids on inpact player and remove one live    
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
enemyBullet w@(World {enemies = listOfEnemies, bullets = listOfBullets, rocks = listOfRocks, asteroidsSpawnGenerator = g}) = w{enemies = map (fst.check) listOfEnemies,bullets = map check1 listOfBullets, rocks = listOfRocks ++  (concatMap (snd.check) listOfEnemies) }
    where
        check enemy@(Enemy{enemyLocation = el }) | all (==False) (map (collisionEnemyBullet enemy) listOfBullets) = (enemy,[])
                    | otherwise = (enemy{enemyStatus = Destroyed},spawnClusterFromShip el g )
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
removeDestroidObjects w@World {asteroids = listOfAsteroids, bullets = listOfBullets, enemies = listofEnemies, rocks = listOfRocks} = w{
                    asteroids   = [a | a@Asteroid {asteroidStatus = NotDestroyed} <- listOfAsteroids],
                    bullets     = [b | b@Bullet {bulletStatus = NotDestroyed} <- listOfBullets],
                    enemies     = [e | e@Enemy {enemyStatus = NotDestroyed} <- listofEnemies],
                    rocks       = [r | r@(Rock {rockStatus = NotDestroyed}) <- listOfRocks]
    }
    
-- File handling
-- Write the current score to the "highscores.txt" file
scoreToTXT :: GameState -> IO GameState
scoreToTXT gstate@(GameState{world = w@(World {score = p})})  = 
    do
        name <- getLine
        f <- readFile "highscores.txt"
        let scores = f ++ ['\n'] ++ show p ++ name
        when (length scores > 0) $ writeFile "highscores.txt" scores
        return gstate
-- extract scores from .txt file
getScoreFromTXT :: IO [String]
getScoreFromTXT = 
    do
        f <- readFile "highscores.txt"
        getsrc f 
    where
        getsrc c | length c > 1 = do
                                let content =  lines c
                                    highScoreTuple = map getHighScoreTuple content
                                    bestScores = take 3 (sortBy (flip compare `on` fst) highScoreTuple) -- sorted list of highscores
                                    scores = map scoreString bestScores
                                return (reverse scores)
                | otherwise = return []
        
-- make string with score
scoreString :: (Int, String) -> String
scoreString (x,y) = y ++ ": " ++ show x
-- make tuple with highscore
getHighScoreTuple :: String -> (Int, String)
getHighScoreTuple scoreNameTuple = (score, name)
    where [(score, name)] = reads scoreNameTuple

-- Random numbers 
    -- function to generate 3 random numbers
generateTreeNumbers :: RandomGen g => g -> ((Float, Float, Float), g)
generateTreeNumbers g = let (v1, g1) = randomR (-200, 200) g
                            (v2, g2) = randomR (1, 5) g1 -- Use new seed
                            (v3, g3) = randomR (1, 20) g2 -- Use new seed
                       in ((v1, v2,v3), g3) -- Return last seed
-- extract first random number
getFirstNumber :: RandomGen g => ((Float, Float,Float), g) -> Float
getFirstNumber ((f, _,_), _) = f
-- extract second random number
getSecondNumber :: RandomGen g => ((Float, Float,Float), g) -> Float
getSecondNumber ((_, f,_), _) = f
-- extract Third random number
getThirdNumber :: RandomGen g => ((Float, Float,Float), g) -> Float
getThirdNumber ((_, _,f), _) = f
-- extract the seed
getSeed :: RandomGen g => ((Float, Float,Float), g) -> g
getSeed ((_, _,_), g) = g
-- generate vector for asteroid
generateVectorAsteroid :: RandomGen g => g -> ((Float, Float), g)
generateVectorAsteroid g = let (v1, g1) = randomR (-4, 4) g
                               (v2, g2) = randomR (-1, -5) g1 -- Use new seed
                           in  ((v1, v2), g2) -- Return last seed
-- extract the vector af an asteroid
getVectorAsteroid :: RandomGen g => ((Float, Float), g) -> (Float, Float)
getVectorAsteroid ((x, y), _) = (x,y)
-- generate the vector for a cluster of rocks
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
-- extract the different vectors
getFirstVector :: RandomGen g => ((Vector,Vector,Vector,Vector,Vector), g) -> Vector
getFirstVector ((v,_,_,_,_), _) = v
getSecondVector :: RandomGen g => ((Vector,Vector,Vector,Vector,Vector), g) -> Vector
getSecondVector ((_,v,_,_,_), _) = v
getThirdVector :: RandomGen g => ((Vector,Vector,Vector,Vector,Vector), g) -> Vector
getThirdVector ((_,_,v,_,_), _) = v
getFourthVector :: RandomGen g => ((Vector,Vector,Vector,Vector,Vector), g) -> Vector
getFourthVector ((_,_,_,v,_), _) = v
getFifthVector :: RandomGen g => ((Vector,Vector,Vector,Vector,Vector), g) -> Vector
getFifthVector ((_,_,_,_,v), _) = v
