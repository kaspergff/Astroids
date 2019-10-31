module Main where

    import Controller
    import Model
    import View
    import System.Random
    
    import Graphics.Gloss.Interface.IO.Game
    

    main :: IO () 
    main = do
        esp <- getStdGen
        otg <- getStdGen
        playIO (InWindow "Astroids" (400, 400) (0, 0)) -- Or FullScreen
                  black            -- Background color
                  60               -- Frames per second
                  (initialState esp otg)     -- Initial state
                  view             -- View function
                  input            -- Event function
                  step             -- Step function