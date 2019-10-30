module Main where

    import Controller
    import Model
    import View
    import System.Random
    
    import Graphics.Gloss.Interface.IO.Game
    

    main :: IO () 
    main = do
        esp <- getStdGen
        playIO (InWindow "Astroids" (400, 400) (0, 0)) -- Or FullScreen
                  black            -- Background color
                  30               -- Frames per second
                  (initialState esp)     -- Initial state
                  view             -- View function
                  input            -- Event function
                  step             -- Step function