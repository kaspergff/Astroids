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
                enemies     ::  [Enemy],
                asteroidTimer   :: Int,
                enemyTimer :: Int,
                asteroidsSpawnGenerator :: StdGen,
                oneThreeGenerator :: StdGen,
                oneFiveGenerator :: StdGen,
                score         :: Int,
                lives         ::  Int,
                livespr       :: Picture,
                eSprite :: Picture
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
                --voor later zorg
                --kunnen we mee voor zorgen dat player enkel dood gaat als hij tegen een bullet van de vijand aanvliegt
                --bulletallegiance :: alliedOrNot
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

    data DestroyedOrNot = Destroyed | NotDestroyed deriving (Eq)
    data Aliveornot = Alive | Dead deriving (Eq)
    
    -- moest pla toevoegen om te laten werken is mis inpure
    initial_world :: StdGen -> StdGen -> StdGen -> Picture -> Picture -> Picture -> World
    initial_world esg otg ofg pla liv ene = World (Player (0,-180) NoMovement pla Alive) Playing [] [] [] [] 0 0 esg otg ofg 0 3 liv ene 
                
    initialState :: StdGen -> StdGen -> StdGen -> Picture -> Picture -> Picture -> GameState
    initialState esg otg ofg pla liv ene = GameState (ShowWorld(initial_world esg otg ofg pla liv ene)) (initial_world esg otg ofg pla liv ene) 0 