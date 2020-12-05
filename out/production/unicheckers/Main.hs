module Main (
    main
) where

import AI
import System.Exit (die)
import State
import Data.Foldable (find)
import Data.Maybe (isJust, fromMaybe, catMaybes, mapMaybe)
import UI.NCurses
import Control.Monad (when)
import Debug.Trace

main :: IO ()
main = runCurses $ do
    setEcho False
    setCursorMode CursorInvisible

    let initial = State { turn    = Blues
                        , cursor  = (3, 3)
                        , figures = generateFigures
                        , isFixed = False
                        , aiTeam  = Reds
                        , level   = 1
                        , isDebug = True
                        }

    c0 <- newColorID ColorWhite ColorBlack $ toInteger idleBlack
    c1 <- newColorID ColorBlack ColorWhite $ toInteger idleWhite
    c2 <- newColorID ColorBlue ColorBlack $ toInteger blueBlack
    c3 <- newColorID ColorBlue ColorWhite $ toInteger blueWhite
    c4 <- newColorID ColorRed ColorBlack $ toInteger redBlack
    c5 <- newColorID ColorRed ColorWhite $ toInteger redWhite

    let colors = [defaultColorID, c0, c1, c2, c3, c4, c5]

    win      <- newWindow 26 44 0 0
    (sh, sw) <- screenSize
    dwin     <- newWindow (sh - 28) sw 28 0

    loop win dwin colors initial
        where
            loop win dwin colors state = do
                when (isDebug state) $ updateDebug dwin state
                update win colors state
                
                when (turn state == aiTeam state) $ loop win dwin colors $ handleAI state
                
                e <- getEvent win Nothing
                case e of
                    Just (EventSpecialKey (KeyFunction 10)) -> do
                        cloneWindow win
                        cloneWindow dwin
                        return ()
                    Just e                                  -> loop win dwin colors $ handleControl state e
                    _                                       -> loop win dwin colors state


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
--                King    -> drawString "███"
                King    -> drawString (show x ++ (if figure `elem` snd (fromMaybe (state, []) $ turnResult state) then "X" else "█") ++ show y)
                Checker -> do
                    drawString (show x)
                    if figure `elem` snd (fromMaybe (state, []) $ turnResult state) then drawString "X" else drawGlyph glyphBoard
                    drawString (show y)
--                    drawGlyph glyphBoard
--                    drawGlyph glyphBoard
--                    drawGlyph glyphBoard
            setAttribute AttributeBlink False
            setColor $ if isWhiteCell x y then colors!!idleWhite else colors!!idleBlack
        Nothing -> drawString $ if any isSelected (figures state) && isJust (turnResult (state {cursor = (x, y)})) then " + " else "   "
--        Nothing -> drawString $ (show x ++ (if any isSelected (figures state) && isJust (turnResult (state {cursor = (x, y)})) then "+" else " ") ++ show y)
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


generateFigures :: [Figure]
generateFigures = catMaybes $ [1..8] >>= \x -> [1..8] >>= \y -> return $ generateMaybeFigure x y


generateMaybeFigure :: Int -> Int -> Maybe Figure
generateMaybeFigure x y
    | x /= 4 && x /= 5 && mod x 2 == mod y 2 = Just Figure { fTeam  = if x <=3 then Blues else Reds
                                                           , fType  = Checker
                                                           , fCell  = (x, y)
                                                           , isSelected = False
                                                           }
    | otherwise = Nothing


-- DEBUG

updateDebug :: Window -> State -> Curses ()
updateDebug dw s = do
    (sh, sw) <- screenSize
    updateWindow dw $ drawDebug s sh sw
    render


dummyFigure :: (Int, Int) -> Figure
dummyFigure c = Figure Blues Checker c False


drawDebug :: State -> Integer -> Integer -> Update ()
drawDebug s sh sw = do
    clear
    drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    moveCursor 0 (sw `quot` 2 - 3)
    drawString " DEBUG "
    moveCursor 2 2
    drawString $ "Selected:\t" ++ show (getSelectedFigure s)
    moveCursor 3 2
    drawString $ "Cursor:\t" ++ show (cursor s)
    moveCursor 4 2
    drawString $ "Full path:\t" ++ show (fullPath (fCell $ fromMaybe (dummyFigure $ cursor s) $ getSelectedFigure s) (cursor s))
    moveCursor 5 2
    drawString $ "Path:\t\t" ++ show (fromMaybe [] $ getPath s)
    moveCursor 6 2
    drawString $ "Will eat:\t" ++ show (snd $ fromMaybe (s, []) $ turnResult s)
    moveCursor 7 2
    drawString $ "Can eat:\t" ++ show (map getSelectedFigure $ filter canEat $ mapMaybe (selectFigure s) (getTeamFigures s (turn s)))

