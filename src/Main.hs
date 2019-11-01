module Main where

    import Controller
    import Model
    import View
    import System.Random
    import Graphics.Gloss.Data.Bitmap
    import Graphics.Gloss.Interface.IO.Game
    
    

    main :: IO () 
    main = do       
        planeafb <- loadBMP "sprites als bitmap/plane.bmp" --load bitmap
        liveafb <- loadBMP "sprites als bitmap/heart.bmp"
        ofg <- getStdGen
        otg <- getStdGen
        esp <- getStdGen
        playIO (InWindow "Astroids" (400, 400) (0, 0)) -- Or FullScreen
                  black            -- Background color
                  60              -- Frames per second
                  (initialState esp otg ofg planeafb liveafb)     -- Initial state 
                  view             -- View function
                  input            -- Event function
                  step             -- Step function