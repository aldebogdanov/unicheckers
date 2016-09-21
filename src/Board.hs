module Board (
    drawBoard
) where

import System.Console.ANSI
import Field

drawBoard :: Cursor -> IO ()
drawBoard c = do
    clearScreen
    putStrLn ""
    sequence_ $ drawLine c <$> [1..8]
    putStrLn ""

drawTopCell :: Cursor -> Int -> Int -> IO ()
drawTopCell c x y
    | fst c == x && snd c == y = do
        setBackColors x y
        setSGR [SetColor Foreground Dull Black]
        putStr "┏  ┓"
        setSGR [Reset]
    | otherwise = do
        setBackColors x y
        putStr "    "

drawBottomCell :: Cursor -> Int -> Int -> IO ()
drawBottomCell c x y
    | fst c == x && snd c == y = do
        setBackColors x y
        setSGR [SetColor Foreground Dull Black]
        putStr "┗  ┛"
        setSGR [Reset]
    | otherwise = do
        setBackColors x y
        putStr "    "

setBackColors :: Int -> Int -> IO ()
setBackColors x y
    | mod x 2 == mod y 2 = setSGR [SetColor Background Vivid White]
    | otherwise = setSGR [SetColor Background Vivid Black]

drawLine :: Cursor -> Int -> IO ()
drawLine c y = do
    putStr "  "
    sequence_ $ (\x -> drawTopCell c x y) <$> [1..8]
    setSGR [Reset]
    putStrLn ""
    putStr "  "
    sequence_ $ (\x -> drawBottomCell c x y) <$> [1..8]
    setSGR [Reset]
    putStrLn ""
