-- | This module contains the data types
--   which represent the state of the game
module Model where

    import System.Random
    import Graphics.Gloss

    data InfoToShow = ShowNothing
                    | ShowWorld World
                    | ShowDeathscreen World
    
    nO_SECS_BETWEEN_CYCLES :: Float
    nO_SECS_BETWEEN_CYCLES = 5
    
    data GameState = GameState {
                       infoToShow  :: InfoToShow
                     , world       :: World
                     , elapsedTime :: Float
                     , scoreSaved  :: Bool
                     
                     }

    data World = World {
                player  :: Player,
                pause   :: PauseorPlay,
                bullets     :: [Bullet],
                rockets     :: [Rocket],
                asteroids   :: [Asteroid],
                enemies     ::  [Enemy],
                asteroidTimer   :: Int,
                enemyTimer :: Int,
                asteroidsSpawnGenerator :: StdGen,
                oneThreeGenerator :: StdGen,
                oneFiveGenerator :: StdGen,
                score         :: Int,
                lives         ::  Int,
                sprites       :: [Picture]
                }


    -- player
    data Player = Player {
                playerlocation  :: (Float,Float),
                movement        :: Movement,
                -- moest toevoegen om te laten werken is mis inpure
                isdead          :: Aliveornot
                }

    -- bullets

    data Bullet = Bullet {
                bulletLocation :: (Float, Float),            
                --bmovement :: Movement
                speed :: Float,
                bulletStatus :: DestroyedOrNot
               
                --bulletallegiance :: AlliedOrNot
                }

    data Rocket = Rocket {
                rockLocation :: (Float, Float),            
                --bmovement :: Movement
                rspeed :: Float,
                rocketStatus :: DestroyedOrNot
                }

    data Asteroid = Asteroid {
                    location		:: (Float,Float),
                    --direction_angle :: Float,
                    size            :: Float,
                    status  :: DestroyedOrNot,
                    aSpeed           :: Float
                    }

    data Enemy = Enemy {
                    enemyLocation :: (Float,Float),
                    estatus :: DestroyedOrNot,
                    espeed :: Float
                    }

    -- movement player
    data Movement = NoMovement | LeftMovement | RightMovement | DownMovement | UpMovement | UpleftMovement | UprightMovement | DownleftMovement| DownrightMovement deriving (Eq)

    -- pause the game
    data PauseorPlay = Paused | Playing deriving (Eq)

    data DestroyedOrNot = Destroyed | NotDestroyed deriving (Eq)--for flying enemies and asteroids
    data Aliveornot = Alive | Dead deriving (Eq)--for the player
    
    -- moest pla toevoegen om te laten werken is mis inpure
    initial_world :: StdGen -> StdGen -> StdGen -> [Picture] -> World
    initial_world esg otg ofg spr = World (Player (0,-180) NoMovement Alive) Playing [] [] [] [] 0 0 esg otg ofg 0 3 spr 
                
    initialState :: StdGen -> StdGen -> StdGen -> [Picture] -> GameState
    initialState esg otg ofg spr = GameState (ShowWorld(initial_world esg otg ofg spr)) (initial_world esg otg ofg spr) 0 False