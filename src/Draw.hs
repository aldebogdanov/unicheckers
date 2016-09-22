module Draw (
    drawBoard
) where

import System.Console.ANSI
import Board

drawBoard :: Cursor -> [Figure] -> IO ()
drawBoard c figs = do
    clearScreen
    putStrLn ""
    sequence_ $ drawLine c figs <$> [1..8]
    putStrLn ""

drawTopCell :: Cursor -> [Figure] -> Int -> Int -> IO ()
drawTopCell c figs x y = do
    setBackColors x y
    drawCellEdge c x y "┏"
    drawInnerCell figs x y "▄▄"
    drawCellEdge c x y "┓"
    setSGR [Reset]

drawBottomCell :: Cursor -> [Figure] -> Int -> Int -> IO ()
drawBottomCell c figs x y = do
    setBackColors x y
    drawCellEdge c x y "┗"
    drawInnerCell figs x y "▀▀"
    drawCellEdge c x y "┛"
    setSGR [Reset]

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
        fig = getFigure figs x y
        setColor Reds = setSGR [SetColor Foreground Vivid Red]
        setColor Blues = setSGR [SetColor Foreground Vivid Blue]

getFigure :: [Figure] -> Int -> Int -> Maybe Figure
getFigure figs x y
    | null fig = Nothing
    | otherwise = Just $ head fig
    where
        fig = filter (\fg -> figureX fg == x && figureY fg == y) figs

setBackColors :: Int -> Int -> IO ()
setBackColors x y
    | mod x 2 == mod y 2 = setSGR [SetColor Background Vivid White]
    | otherwise = setSGR [SetColor Background Vivid Black]

drawLine :: Cursor -> [Figure] -> Int -> IO ()
drawLine c figs y = do
    putStr "  "
    sequence_ $ (\x -> drawTopCell c figs x y) <$> [1..8]
    setSGR [Reset]
    putStrLn ""
    putStr "  "
    sequence_ $ (\x -> drawBottomCell c figs x y) <$> [1..8]
    setSGR [Reset]
    putStrLn ""
