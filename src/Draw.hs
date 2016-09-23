module Draw (
    drawBoard
) where

import System.Console.ANSI
import Board

drawBoard :: State -> IO ()
drawBoard state = do
    clearScreen
    putStrLn ""
    sequence_ $ drawLine state <$> [1..8]
    putStrLn ""

drawTopCell :: State -> Int -> Int -> IO ()
drawTopCell state x y = do
    setBackColors x y
    drawCellEdge c x y "┏"
    drawInnerCell figs x y str
    drawCellEdge c x y "┓"
    setSGR [Reset]
    where
        c = cursor state
        figs = figures state
        str = if selection state == Just (x, y) then "⢠⡄" else "▗▖"

drawBottomCell :: State -> Int -> Int -> IO ()
drawBottomCell state x y = do
    setBackColors x y
    drawCellEdge c x y "┗"
    drawInnerCell figs x y str
    drawCellEdge c x y "┛"
    setSGR [Reset]
    where
        c = cursor state
        figs = figures state
        str = if selection state == Just (x, y) then "⠘⠃" else "▝▘"

drawCellEdge :: Cursor -> Int -> Int -> String -> IO ()
drawCellEdge c x y str
    | fst c == x && snd c == y = do
        setSGR [SetColor Foreground Dull Black]
        putStr str
    | otherwise = putStr " "

drawInnerCell :: [Figure] -> Int -> Int -> String -> IO ()
drawInnerCell figs x y str =
    case fig of
        Nothing -> putStr "  "
        Just figure -> do
            setColor $ figureTeam figure
            putStr str
    where
        fig = getFigureFromList figs (x, y)
        setColor Reds = setSGR [SetColor Foreground Vivid Red]
        setColor Blues = setSGR [SetColor Foreground Vivid Blue]

setBackColors :: Int -> Int -> IO ()
setBackColors x y
    | mod x 2 == mod y 2 = setSGR [SetColor Background Vivid White]
    | otherwise = setSGR [SetColor Background Vivid Black]

drawLine :: State -> Int -> IO ()
drawLine state y = do
    putStr "  "
    sequence_ $ (\x -> drawTopCell state x y) <$> [1..8]
    setSGR [Reset]
    putStrLn ""
    putStr "  "
    sequence_ $ (\x -> drawBottomCell state x y) <$> [1..8]
    setSGR [Reset]
    putStrLn ""
