-- | This module defines how the state changes
--   in response to time and user input
module Controller where

    import Model
    
    import Graphics.Gloss
    import Graphics.Gloss.Interface.IO.Game
    import System.Random
    
    -- | Handle one iteration of the game
    step :: Float -> GameState -> IO GameState
    step secs gstate
      | elapsedTime gstate + secs > nO_SECS_BETWEEN_CYCLES
      = return $ gstate { elapsedTime = elapsedTime gstate + secs, infoToShow = ShowWorld(updateWorld $ world gstate), world = updateWorld $ world gstate}
      | otherwise
      = -- Just update the elapsed time
        return $ gstate { elapsedTime = elapsedTime gstate + secs }
    
    -- | Handle user input
 
    input :: Event -> GameState -> IO GameState
    input e gstate = return (inputKey e gstate)
    
    inputKey :: Event -> GameState -> GameState
    inputKey _ gstate@(GameState{world = w}) = gstate {world = w}



    updateWorld :: World -> World
    updateWorld w = w


    updatePlane :: World -> World
    updatePlane w = w