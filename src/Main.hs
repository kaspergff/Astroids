module Main where

    import Controller
    import Model
    import View
    
    import Graphics.Gloss.Interface.IO.Game
    
    
    main :: IO ()
    main = playIO (InWindow "Astroids" (400, 400) (0, 0)) -- Or FullScreen
                  green            -- Background color
                  30               -- Frames per second
                  initialState     -- Initial state
                  view             -- View function
                  input            -- Event function
                  step             -- Step function