module Main (
    main
) where

import AI
import Board
import Draw
import System.Exit (die)
import State
import Data.Foldable (find)
import UI.NCurses

main :: IO ()
main = runCurses $ do
    setEcho False
    setCursorMode CursorInvisible

    let initial = State { turn = Blues
                        , cursor = (3, 3)
                        , figures = generateFigures
                        }

    c0 <- newColorID ColorWhite ColorBlack $ toInteger idleBlack
    c1 <- newColorID ColorBlack ColorWhite $ toInteger idleWhite
    c2 <- newColorID ColorBlue ColorBlack $ toInteger blueBlack
    c3 <- newColorID ColorBlue ColorWhite $ toInteger blueWhite
    c4 <- newColorID ColorRed ColorBlack $ toInteger redBlack
    c5 <- newColorID ColorRed ColorWhite $ toInteger redWhite

    let colors = [defaultColorID, c0, c1, c2, c3, c4, c5]
    w <- newWindow 26 44 0 0

    loop w colors initial
        where
            loop w colors state = do
                update w colors state
                render
                e <- getEvent w Nothing
                case e of
                    Just (EventSpecialKey (KeyFunction 10)) -> return () -- closeWindow w
                    Just e                                  -> loop w colors $ handleControl state e
                    _                                       -> loop w colors state


update :: Window -> [ColorID] ->  State -> Curses ()
update w colors state = do
    updateWindow w $ drawCells colors state
    render


drawCells :: [ColorID] -> State -> Update ()
drawCells colors state = do
    setColor $ colors!!idleBlack
    sequence_ $ drawAScaleItem <$> zip [1 .. 8] ['a' .. 'h']
    sequence_ $ drawNScaleItem <$> [1 .. 8]
    sequence_ $ drawCell colors state <$> [ (x, y) | x <- [1 .. 8], y <- [1..8] ]


drawAScaleItem :: (Int, Char) -> Update ()
drawAScaleItem (n, ch) = do
    moveCursor 0 (fromIntegral $ (n-1) * 5 + 4)
    drawString [ch]
    moveCursor 25 (fromIntegral $ (n-1) * 5 + 4)
    drawString [ch]


drawNScaleItem :: Int -> Update ()
drawNScaleItem n = do
    moveCursor (fromIntegral $ (8-n) * 3 + 2) 0
    drawString $ show n
    moveCursor (fromIntegral $ (8-n) * 3 + 2) 43
    drawString $ show n


drawCell :: [ColorID] -> State -> (Int, Int) -> Update ()
drawCell colors state (x, y) = do
    setAttribute AttributeBold True
    moveCursor (fromIntegral $ (8-x) * 3 + 1) (fromIntegral $ (y-1) * 5 + 2)
    setColor $ if isWhiteCell x y then colors!!idleWhite else colors!!idleBlack
    if cursor state == (x, y)
        then do
            drawGlyph glyphCornerUL
            drawGlyph glyphLineH
            drawGlyph glyphLineH
            drawGlyph glyphLineH
            drawGlyph glyphCornerUR
        else drawString "     "
    moveCursor (fromIntegral $ (8-x) * 3 + 2) (fromIntegral $ (y-1) * 5 + 2)
    if cursor state == (x, y)
        then drawGlyph glyphLineV
        else drawString " "
    case find (\fig -> fCell fig == (x, y)) $ figures state of
        Just figure -> do
            case fTeam figure of
                Blues -> setColor $ if isWhiteCell x y then colors!!blueWhite else colors!!blueBlack
                Reds  -> setColor $ if isWhiteCell x y then colors!!redWhite else colors!!redBlack
            setAttribute AttributeBlink $ isSelected figure
            case fType figure of
                Checker -> drawString "███"
                King    -> do
                    drawGlyph glyphDiamond
                    drawGlyph glyphDiamond
                    drawGlyph glyphDiamond
            setAttribute AttributeBlink False
            setColor $ if isWhiteCell x y then colors!!idleWhite else colors!!idleBlack
        -- TODO: Make it type safe (isTurnAllowed use head and falls on empty list). Now it is not a Haskell way
        Nothing -> drawString $ if any isSelected (figures state) && isTurnAllowed (state { cursor = (x, y) }) then " + " else "   "
    if cursor state == (x, y)
        then drawGlyph glyphLineV
        else drawString " "
    moveCursor (fromIntegral $ (8-x) * 3 + 3) (fromIntegral $ (y-1) * 5 + 2)
    if cursor state == (x, y)
        then do
            drawGlyph glyphCornerLL
            drawGlyph glyphLineH
            drawGlyph glyphLineH
            drawGlyph glyphLineH
            drawGlyph glyphCornerLR
        else drawString "     "
    setColor $ colors!!idleBlack
    drawString " "


isWhiteCell :: Int -> Int -> Bool
isWhiteCell x y = odd $ x + y

idleBlack :: Int
idleBlack = 1

idleWhite :: Int
idleWhite = 2

blueBlack :: Int
blueBlack = 3

blueWhite :: Int
blueWhite = 4

redBlack :: Int
redBlack = 5

redWhite :: Int
redWhite = 6
