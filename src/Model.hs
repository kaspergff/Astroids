-- | This module contains the data types
--   which represent the state of the game
module Model where

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
                asteroids   :: [Asteroid]
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
                speed :: Float
               }

    data Asteroid = Asteroid {
                    location		:: (Float,Float)
                    --direction_angle :: Float,
                    --speed           :: Float,
                    --size            :: Int
                    --asteroid_state  :: State
                    }



    -- movement player
    data Movement = NoMovement | LeftMovement | RightMovement | DownMovement | UpMovement  deriving (Eq)

    -- pause the game
    data PauseorPlay = Paused | Playing deriving (Eq)
                               
    initial_world :: World
    initial_world = World (Player (0,-180) NoMovement) Playing [] []
                
    initialState :: GameState
    initialState = GameState (ShowWorld(initial_world)) initial_world 0 