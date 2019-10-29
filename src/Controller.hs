-- | This module defines how the state changes
--   in response to time and user input
module Controller where

    import Model
    
    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    import System.Random
    import System.Environment
    
    -- | Handle one iteration of the game
    step :: Float -> GameState -> IO GameState
    step secs gstate = return $ gstate { elapsedTime = elapsedTime gstate + secs, infoToShow = ShowWorld(updateWorld $ world gstate), world = updateWorld $ world gstate}
    
    -- | Handle user input
 
    input :: Event -> GameState -> IO GameState
    input e gstate = return (inputKey e gstate)

    inputKey :: Event -> GameState -> GameState                                             
    inputKey (EventKey (SpecialKey (KeyLeft)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = LeftMovement}}}
    inputKey (EventKey (SpecialKey (KeyRight)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = RightMovement}}}
    inputKey (EventKey (SpecialKey (KeyUp)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = UpMovement}}}
    inputKey (EventKey (SpecialKey (KeyDown)) Down _ _) gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = DownMovement}}}    
    inputKey _ gstate@(GameState _ w@(World{player = p}) _) = gstate {world = w {player = p {movement = NoMovement}}}



    updateWorld :: World -> World
    updateWorld w = updatePlane $ planeOnScreen w


    updatePlane :: World -> World
    updatePlane w@(World(Player {playerlocation = (x,y), movement = dir}))
        | dir == RightMovement = World(Player {playerlocation = (x + 10 ,y), movement = RightMovement})
        | dir == LeftMovement = World(Player {playerlocation = (x - 10,y), movement = LeftMovement})
        | dir == DownMovement = World(Player {playerlocation = (x,y - 10), movement = DownMovement})
        | dir == UpMovement = World(Player {playerlocation = (x,y + 10), movement = UpMovement})
        | dir == NoMovement = w

    planeOnScreen :: World -> World
    planeOnScreen w@(World p@(Player {playerlocation = (x,y)}))
        | x <= (-194) = w{ player = p {playerlocation = (-194,y)}}
        | x >= (194) = w{ player = p {playerlocation = (194,y)}}
        | y <= (-190) = w{ player = p {playerlocation = (x,-190)}}
        | y >= (170) = w{ player = p {playerlocation = (x,170)}}
        | otherwise = w 

