module Main (
    main
) where

import AI
import State
import Data.Foldable (find)
import Data.Maybe (isJust, fromMaybe, mapMaybe)
import UI.NCurses
import Control.Monad (when)

main :: IO ()
main = runCurses $ do
    setEcho False
    setCursorMode CursorInvisible

    let initial = State { status    = Stopped
                        , lastVar   = []
                        , turn      = Blues
                        , cursor    = (3, 3)
                        , figures   = [] -- generateFigures
                        , isFixed   = False
                        , aiTeam    = Reds
                        , level     = 1
                        , winner    = Nothing
                        , inOptions = True
                        , option    = 2
                        , isDebug   = False
                        , variants  = []
                        }

    c0 <- newColorID ColorWhite ColorBlack $ toInteger idleBlack
    c1 <- newColorID ColorBlack ColorWhite $ toInteger idleWhite
    c2 <- newColorID ColorBlue ColorBlack $ toInteger blueBlack
    c3 <- newColorID ColorBlue ColorWhite $ toInteger blueWhite
    c4 <- newColorID ColorRed ColorBlack $ toInteger redBlack
    c5 <- newColorID ColorRed ColorWhite $ toInteger redWhite

    let colors = [defaultColorID, c0, c1, c2, c3, c4, c5]

    win      <- newWindow 28 46 0 0
    (sh, sw) <- screenSize
    owin     <- newWindow 28 31 0 46
    dwin     <- newWindow (sh - 28) sw 28 0

    loop win owin dwin colors initial
        where
            loop win owin dwin colors state = do

                update win colors state
                updateOptions owin colors state
                updateDebug dwin state

                when (status state == InProcess && turn state == aiTeam state) $ loop win owin dwin colors $ handleAI state
--                when (status state == InProcess) $ loop win owin dwin colors $ handleAI state

                e <- getEvent win Nothing
                case e of
                    Just (EventSpecialKey (KeyFunction 10)) -> do
                        closeWindow win
                        cloneWindow owin
                        closeWindow dwin
                        return ()
                    Just (EventCharacter '\t') -> loop win owin dwin colors $ state { inOptions = not (inOptions state) }
                    Just e'                    -> loop win owin dwin colors $
                                                  if inOptions state
                                                      then handleOptionsControl state e'
                                                      else handleGameControl state e'
                    _                          -> loop win owin dwin colors state


update :: Window -> [ColorID] ->  State -> Curses ()
update w colors state = do
    updateWindow w $ drawCells colors state
    render


drawCells :: [ColorID] -> State -> Update ()
drawCells colors state = do
    setColor $ colors!!idleBlack
    drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    moveCursor 0 20
    setColor $ if inOptions state then colors!!idleBlack else colors!!idleWhite
    drawString " GAME "
    setColor $ colors!!idleBlack
    sequence_ $ drawAScaleItem <$> zip [1 .. 8] ['a' .. 'h']
    sequence_ $ drawNScaleItem <$> [1 .. 8]
    sequence_ $ drawCell colors state <$> [ (x, y) | x <- [1 .. 8], y <- [1..8] ]


drawAScaleItem :: (Int, Char) -> Update ()
drawAScaleItem (n, ch) = do
    moveCursor 1 (fromIntegral $ (n-1) * 5 + 5)
    drawString [ch]
    moveCursor 26 (fromIntegral $ (n-1) * 5 + 5)
    drawString [ch]


drawNScaleItem :: Int -> Update ()
drawNScaleItem n = do
    moveCursor (fromIntegral $ (8-n) * 3 + 3) 1
    drawString $ show n
    moveCursor (fromIntegral $ (8-n) * 3 + 3) 44
    drawString $ show n


drawCell :: [ColorID] -> State -> (Int, Int) -> Update ()
drawCell colors state (x, y) = do
    setAttribute AttributeBold True
    moveCursor (fromIntegral $ (8-x) * 3 + 2) (fromIntegral $ (y-1) * 5 + 3)
    setColor $ if isWhiteCell x y then colors!!idleWhite else colors!!idleBlack
    if cursor state == (x, y)
        then do
            drawGlyph glyphCornerUL
            drawGlyph glyphLineH
            drawGlyph glyphLineH
            drawGlyph glyphLineH
            drawGlyph glyphCornerUR
        else drawString "     "
    moveCursor (fromIntegral $ (8-x) * 3 + 3) (fromIntegral $ (y-1) * 5 + 3)
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
                King    -> if isDebug state then
                    drawString (
                        show x ++ (if figure `elem` snd (fromMaybe (state, []) $ turnResult state) then "X" else "█") ++ show y
                    ) else drawString "███"
                Checker -> (if isDebug state then
                                (do drawString (show x)
                                    if figure
                                         `elem` snd (fromMaybe (state, []) $ turnResult state) then
                                        drawString "X"
                                    else
                                        drawGlyph glyphBoard
                                    drawString (show y))
                            else
                                (do drawGlyph glyphBoard
                                    drawGlyph glyphBoard
                                    drawGlyph glyphBoard))
            setAttribute AttributeBlink False
            setColor $ if isWhiteCell x y then colors!!idleWhite else colors!!idleBlack
        Nothing -> drawString $ if any isSelected (figures state) && isJust (turnResult (state {cursor = (x, y)})) then " + " else "   "
    if cursor state == (x, y)
        then drawGlyph glyphLineV
        else drawString " "
    moveCursor (fromIntegral $ (8-x) * 3 + 4) (fromIntegral $ (y-1) * 5 + 3)
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


-- OPTIONS

updateOptions :: Window -> [ColorID] -> State -> Curses ()
updateOptions ow colors s = do
    updateWindow ow $ drawOptions colors s
    render


drawOptions :: [ColorID] -> State -> Update ()
drawOptions colors s = do
    setColor $ colors!!idleBlack
    drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    moveCursor 0 11
    setColor $ if inOptions s then colors!!idleWhite else colors!!idleBlack
    drawString " OPTIONS "
    setColor $ colors!!idleBlack
    moveCursor 2 2
    setColor $ colors!!redBlack
    drawString "██"
    setColor $ colors!!idleBlack
    moveCursor 2 5
    drawTeamString s Reds
    moveCursor 4 2
    setColor $ colors!!blueBlack
    drawString "██"
    setColor $ colors!!idleBlack
    moveCursor 4 5
    drawTeamString s Blues
    setColor $ if option s == 0 then colors!!idleWhite else colors!!idleBlack
    moveCursor 6 2
    drawString "        Change Team        "
    setColor $ if option s == 1 then colors!!idleWhite else colors!!idleBlack
    moveCursor 8 2
    drawString " "
    drawGlyph glyphArrowL
    drawString " "
    moveCursor 8 27
    drawString " "
    drawGlyph glyphArrowR
    drawString " "
    setColor $ colors!!idleBlack
    moveCursor 8 12
    drawString $ "Level: " ++ show (level s)
    setColor $ if option s == 2 then colors!!idleWhite else colors!!idleBlack
    moveCursor 10 2
    drawString "          New Game          "
    setColor $ if option s == 3 then colors!!idleWhite else colors!!idleBlack
    moveCursor 25 2
    drawString "       "
    drawGlyph $ if isDebug s then glyphBlock else glyphBullet
    drawString " Debug  Mode       "
    setColor $ colors!!idleBlack


drawTeamString :: State -> Team -> Update ()
drawTeamString s t  | aiTeam s == t = case winner s of
                                          Just w  -> drawString $ if w == t then "WIN                     "
                                                                            else "LOSE                    "
                                          Nothing -> if status s == InProcess && turn s == aiTeam s
                                                                   then do
                                                                      setAttribute AttributeBlink True
                                                                      drawString "Computing...            "
                                                                      setAttribute AttributeBlink False
                                                                   else
                                                                      drawString "Waiting...              "
                    | otherwise     = case winner s of
                                          Just w  -> drawString $ if w == t then "YOU WIN                 "
                                                                            else "YOU LOSE                "
                                          Nothing -> drawString "YOU                     "



-- DEBUG

updateDebug :: Window -> State -> Curses ()
updateDebug dw s = do
    (_, sw) <- screenSize
    updateWindow dw clear
    when (isDebug s) $ updateWindow dw $ drawDebug s sw
    render


dummyFigure :: (Int, Int) -> Figure
dummyFigure c = Figure Blues Checker c False


drawDebug :: State -> Integer -> Update ()
drawDebug s sw = do
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
    drawString $ "Can eat:\t" ++ show (map getSelectedFigure $ filter canEat $ mapMaybe (selectFigure s) (getCurrentTeamFigures s))
    moveCursor 8 2
    drawString $ "Variants num:\t" ++ show (length $ variants s)
--    moveCursor 9 2
--    drawString $ "Sample variant:\t" ++ show (listToMaybe $ variants s)
