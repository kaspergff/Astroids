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
                     
                     }

    data World = World {
                player  :: Player,
                pause   :: PauseorPlay,
                bullets     :: [Bullet],
                rockets     :: [Rocket],
                asteroids   :: [Asteroid],
                asteroidTimer   :: Int,
                asteroidsSpawnGenerator :: StdGen,
                oneThreeGenerator :: StdGen,
                oneFiveGenerator :: StdGen,
                score         :: Int,
                lives         ::  Int,
                livespr       :: Picture
                }


    -- player
    data Player = Player {
                playerlocation  :: (Float,Float),
                movement        :: Movement,
                -- moest toevoegen om te laten werken is mis inpure
                sprite          :: Picture, 
                isdead          :: Aliveornot
                }

    -- bullets

    data Bullet = Bullet {
                bulletLocation :: (Float, Float),            
                --bmovement :: Movement
                speed :: Float,
                bulletStatus :: DestroyedOrNot

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



    -- movement player
    data Movement = NoMovement | LeftMovement | RightMovement | DownMovement | UpMovement | UpleftMovement | UprightMovement | DownleftMovement| DownrightMovement deriving (Eq)

    -- pause the game
    data PauseorPlay = Paused | Playing deriving (Eq)

    data DestroyedOrNot = Destroyed | NotDestroyed deriving (Eq)
    data Aliveornot = Alive | Dead deriving (Eq)
    
    -- moest pla toevoegen om te laten werken is mis inpure
    initial_world :: StdGen -> StdGen -> StdGen -> Picture -> Picture -> World
    initial_world esg otg ofg pla liv = World (Player (0,-180) NoMovement pla Alive) Playing [] [] [] 0 esg otg ofg 0 3 liv
                
    initialState :: StdGen -> StdGen -> StdGen -> Picture -> Picture -> GameState
    initialState esg otg ofg pla liv = GameState (ShowWorld(initial_world esg otg ofg pla liv)) (initial_world esg otg ofg pla liv) 0 