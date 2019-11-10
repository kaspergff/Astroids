-- | This module contains the data types
--   which represent the state of the game
module Model where

    import System.Random
    import Graphics.Gloss
  
  -- 
    data InfoToShow = ShowNothing
                    | ShowWorld World
                    | ShowDeathscreen World
    
    nO_SECS_BETWEEN_CYCLES :: Float
    nO_SECS_BETWEEN_CYCLES = 5
    
    -- game state
    -- infoTShow = verschillende schermen
    -- world is wereld
    -- scoreSaved is nodig om te zorgen dat de naam van de player maar 1x ingevuld kan worden
    data GameState = GameState {
      infoToShow  :: InfoToShow,
      world       :: World,
      scoreSaved  :: Bool
      }
-- De wereld
    data World = World {
      player                  :: Player, -- Speler
      pause                   :: PauseorPlay, -- Pauze of aan t spelen
      bullets                 :: [Bullet], -- lijst met kogels
      asteroids               :: [Asteroid], -- lijst met asteroids
      enemies                 :: [Enemy], -- lijst met vijanden
      asteroidTimer           :: Int, -- timer voor volgende asteroid
      enemyTimer              :: Int, -- timer voor volgende enemy
      asteroidsSpawnGenerator :: StdGen, -- generator voor random getal
      score                   :: Int, -- score
      lives                   :: Int, -- aantal levens
      sprites                 :: [Picture], -- lijst met sprites
      schooterTimer           :: Int, -- timer voor schot enemies
      rocks                   :: [Rock] -- lijst met rocks voor explosie na vernietegen asteroid
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
    -- data type om bij de houden of een kogel van de player of de enemie komt
    data AlliedOrNot = Allied | Notallied deriving (Eq)
  -- asteroid
    data Asteroid = Asteroid {
      asteroidLocation :: Point,
      asteroidSize     :: Float,
      asteroidStatus   :: DestroyedOrNot,
      asteroidHeading  :: Vector
      }
    -- rocks voor explosie
    data Rock = Rock {
      rockLocation    :: Point,
      rockHeading     :: Vector,
      liveTime        :: Int, -- om te zorgen dat ze na een tijdje verdwijnen.
      rockStatus      :: DestroyedOrNot,
      asteroidOrShip  :: AsteroidOrShip
      }

      -- enemy
    data Enemy = Enemy {
      enemyLocation :: Point,
      enemyStatus   :: DestroyedOrNot,
      enemySpeed    :: Float
      }

    data AsteroidOrShip = FromAsteroid | FromShip deriving (Eq)

    -- movement player
    data Movement = NoMovement | LeftMovement | RightMovement | DownMovement | UpMovement  deriving (Eq)

    -- pause the game
    data PauseorPlay = Paused | Playing deriving (Eq)
    -- om bij te houden of verschillende objecten destroyed zijn of niet
    data DestroyedOrNot = Destroyed | NotDestroyed deriving (Eq)--for flying enemies and asteroids
    -- check of de player leeft of dood is
    data Aliveornot = Alive | Dead deriving (Eq)--for the player
    
    -- de begin wereld
    initialWorld :: StdGen -> [Picture] -> World
    initialWorld esg spr = World (Player (0,-180) NoMovement Alive) Playing [] [] [] 0 0 esg 0 3 spr 40 []
    -- begin state van de game            
    initialState :: StdGen -> [Picture] -> GameState
    initialState esg spr = GameState (ShowWorld(initialWorld esg spr)) (initialWorld esg spr) False

    

   