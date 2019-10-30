-- | This module contains the data types
--   which represent the state of the game
module Model where

    import System.Random

    data InfoToShow = ShowNothing
                    | ShowWorld World
    
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
                asteroids   :: [Asteroid],
                asteroidTimer   :: Int,
                asteroidsSpawnGenerator :: StdGen,
                score         :: Int,
                lives         ::  Int
                }


    -- player
    data Player = Player {
                playerlocation  :: (Float,Float),
                movement        :: Movement
                }

    -- bullets

    data Bullet = Bullet {
                bulletLocation :: (Float, Float),            
                --bmovement :: Movement
                speed :: Float,
                bulletStatus :: DestroyedOrNot

               }

    data Asteroid = Asteroid {
                    location		:: (Float,Float),
                    --direction_angle :: Float,
                    --speed           :: Float,
                    size            :: Float,
                    --asteroid_state  :: State
                    status  :: DestroyedOrNot
                    }



    -- movement player
    data Movement = NoMovement | LeftMovement | RightMovement | DownMovement | UpMovement  deriving (Eq)

    -- pause the game
    data PauseorPlay = Paused | Playing deriving (Eq)

    data DestroyedOrNot = Destroyed | NotDestroyed deriving (Eq)
                               
    initial_world :: StdGen -> World
    initial_world esg = World (Player (0,-180) NoMovement) Playing [] [] 0 esg 0 3
                
    initialState :: StdGen -> GameState
    initialState esg = GameState (ShowWorld(initial_world esg)) (initial_world esg) 0 