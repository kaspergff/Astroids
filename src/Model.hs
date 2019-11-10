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
      infoToShow  :: InfoToShow,
      world       :: World,
      scoreSaved  :: Bool
      }

    data World = World {
      player                  :: Player,
      pause                   :: PauseorPlay,
      bullets                 :: [Bullet],
      asteroids               :: [Asteroid],
      enemies                 :: [Enemy],
      asteroidTimer           :: Int,
      enemyTimer              :: Int,
      asteroidsSpawnGenerator :: StdGen,
      score                   :: Int,
      lives                   :: Int,
      sprites                 :: [Picture],
      schooterTimer           :: Int,
      rocks                   :: [Rock]
      }

    -- player
    data Player = Player {
      playerLocation :: Point,
      movement       :: Movement,
      playerStatus   :: Aliveornot
      }

    -- bullets

    data Bullet = Bullet {
      bulletLocation   :: Point,            
      bulletSpeed      :: Float,
      bulletStatus     :: DestroyedOrNot,
      bulletAllegiance :: AlliedOrNot
      }
    
    data AlliedOrNot = Allied | Notallied deriving (Eq)

    data Asteroid = Asteroid {
      asteroidLocation :: Point,
      asteroidSize     :: Float,
      asteroidStatus   :: DestroyedOrNot,
      asteroidSpeed    :: Float,
      asteroidHeading  :: Vector
      }
    data Rock = Rock {
      rockLocation :: Point,
      rockHeading  :: Vector,
      liveTime     :: Int,
      rockStatus   :: DestroyedOrNot
      }

    data Enemy = Enemy {
      enemyLocation :: Point,
      enemyStatus   :: DestroyedOrNot,
      enemySpeed    :: Float
      }
    
    --data Point = Point Float Float
    --data Vector = Vector Float Float

    -- movement player
    data Movement = NoMovement | LeftMovement | RightMovement | DownMovement | UpMovement | UpleftMovement | UprightMovement | DownleftMovement| DownrightMovement deriving (Eq)

    -- pause the game
    data PauseorPlay = Paused | Playing deriving (Eq)

    data DestroyedOrNot = Destroyed | NotDestroyed deriving (Eq)--for flying enemies and asteroids
    data Aliveornot = Alive | Dead deriving (Eq)--for the player
    
    initialWorld :: StdGen -> [Picture] -> World
    initialWorld esg spr = World (Player (0,-180) NoMovement Alive) Playing [] [] [] 0 0 esg 0 3 spr 40 []
                
    initialState :: StdGen -> [Picture] -> GameState
    initialState esg spr = GameState (ShowWorld(initialWorld esg spr)) (initialWorld esg spr) False

    

   