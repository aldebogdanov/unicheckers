module Board (
    drawBoard
) where

import System.Console.ANSI

drawBoard :: IO ()
drawBoard = do
    clearScreen
    sequence_ $ drawLine <$> [1..8]

drawCell :: Int -> Int -> IO ()
drawCell x y = do
    setColors x y
    putStr "  "
    where
        setColors x y
            | mod x 2 == mod y 2 = setSGR [SetColor Background Vivid White]
            | otherwise = setSGR [SetColor Background Vivid Black]

drawLine :: Int -> IO ()
drawLine y = do
    putStr "  "
    sequence_ $ (`drawCell` y) <$> [1..8]
    setSGR [Reset]
    putStrLn ""
