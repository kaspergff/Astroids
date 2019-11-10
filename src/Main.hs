module Main where

    import Controller
    import Model
    import View
    import System.Random
    import Graphics.Gloss.Data.Bitmap
    import Graphics.Gloss.Interface.IO.Game
    
    
-- main function to start the game.
-- 3 bitmaps worden geladen en een generator voor random getallen wordt gecreeërd
    main :: IO () 
    main = do       
        planeafb <- loadBMP "sprites als bitmap/plane.bmp" --load bitmap for player 
        liveafb <- loadBMP "sprites als bitmap/heart.bmp" --load bitmap for lives
        enemyafb <- loadBMP "sprites als bitmap/enemyplane.bmp" --load bitmap for enemy 
        g <- getStdGen -- random seed generator
        playIO (InWindow "Astroids" (400, 400) (0, 0)) -- Or FullScreen
                  black            -- Background color
                  60              -- Frames per second
                  (initialState g [planeafb, liveafb, enemyafb])     -- Initial state 
                  view             -- View function
                  input            -- Event function
                  step             -- Step function