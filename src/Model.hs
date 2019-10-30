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
                pause   :: PauseorPlay
                }

    -- player            
    data Player = Player {
                playerlocation  :: (Float,Float),
                movement        :: Movement
                }
    -- movement player
    data Movement = NoMovement | LeftMovement | RightMovement | DownMovement | UpMovement  deriving (Eq)

    -- pause the game
    data PauseorPlay = Pause | Play deriving (Eq)
                               
    initial_world :: World
    initial_world = World (Player (0,-180) NoMovement) Play
                


    initialState :: GameState
    initialState = GameState (ShowWorld(initial_world)) initial_world 0 