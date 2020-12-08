{-# LANGUAGE NoImplicitPrelude #-}
module Main (
    main
) where

import Relude
import AI
import GameState
import UI.NCurses


data MyColor
    = IdleBlack
    | IdleWhite
    | BlueBlack
    | BlueWhite
    | RedBlack
    | RedWhite


main :: IO ()
main = runCurses $ do
    setEcho False
    setCursorMode CursorInvisible

    let initial = GameState { status    = Stopped
                        , turn      = Blues
                        , cursor    = (3, 3)
                        , figures   = [] -- generateFigures
                        , isFixed   = False
                        , aiTeam    = Reds
                        , aiCache   = []
                        , level     = 1
                        , winner    = Nothing
                        , inOptions = True
                        , option    = 2
                        , isDebug   = False
                        }

    c0 <- newColorID ColorWhite ColorBlack $ toInteger idleBlack
    c1 <- newColorID ColorBlack ColorWhite $ toInteger idleWhite
    c2 <- newColorID ColorBlue ColorBlack $ toInteger blueBlack
    c3 <- newColorID ColorBlue ColorWhite $ toInteger blueWhite
    c4 <- newColorID ColorRed ColorBlack $ toInteger redBlack
    c5 <- newColorID ColorRed ColorWhite $ toInteger redWhite

    let colors mc = case mc of 
          IdleBlack -> c0
          IdleWhite -> c1
          BlueBlack -> c2
          BlueWhite -> c3
          RedBlack  -> c4
          RedWhite  -> c5

    win      <- newWindow 28 46 0 0
    (sh, sw) <- screenSize
    owin     <- newWindow 28 31 0 46
    dwin     <- newWindow (sh - 28) sw 28 0

    loop win owin dwin colors initial
        where
            loop win owin dwin colors s = do

                update win colors s
                updateOptions owin colors s
                updateDebug dwin s

                when (status s == InProcess && turn s == aiTeam s) $ loop win owin dwin colors $ handleAI s
--                when (status s == InProcess) $ loop win owin dwin colors $ handleAI s

                e <- getEvent win Nothing
                case e of
                    Just (EventSpecialKey (KeyFunction 10)) -> do
                        closeWindow win
                        cloneWindow owin
                        closeWindow dwin
                        pass
                    Just (EventCharacter '\t') -> loop win owin dwin colors $ s { inOptions = not (inOptions s) }
                    Just e'                    -> loop win owin dwin colors $
                                                  if inOptions s
                                                      then handleOptionsControl s e'
                                                      else handleGameControl s e'
                    _                          -> loop win owin dwin colors s


update :: Window -> (MyColor -> ColorID) ->  GameState -> Curses ()
update w colors s = do
    updateWindow w $ drawCells colors s
    render


drawCells :: (MyColor -> ColorID) -> GameState -> Update ()
drawCells colors s = do
    setColor $ colors IdleBlack
    drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    moveCursor 0 20
    setColor $ if inOptions s then colors IdleBlack else colors IdleWhite
    drawString " GAME "
    setColor $ colors IdleBlack
    sequence_ $ drawAScaleItem <$> zip [1 .. 8] ['a' .. 'h']
    sequence_ $ drawNScaleItem <$> [1 .. 8]
    sequence_ $ drawCell colors s <$> [ (x, y) | x <- [1 .. 8], y <- [1..8] ]


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


drawCell :: (MyColor -> ColorID) -> GameState -> (Int, Int) -> Update ()
drawCell colors s (x, y) = do
    setAttribute AttributeBold True
    moveCursor (fromIntegral $ (8-x) * 3 + 2) (fromIntegral $ (y-1) * 5 + 3)
    setColor $ if isWhiteCell x y then colors IdleWhite else colors IdleBlack
    if cursor s == (x, y)
        then do
            drawGlyph glyphCornerUL
            drawGlyph glyphLineH
            drawGlyph glyphLineH
            drawGlyph glyphLineH
            drawGlyph glyphCornerUR
        else drawString "     "
    moveCursor (fromIntegral $ (8-x) * 3 + 3) (fromIntegral $ (y-1) * 5 + 3)
    if cursor s == (x, y)
        then drawGlyph glyphLineV
        else drawString " "
    case find (\fig -> fCell fig == (x, y)) $ figures s of
        Just figure -> do
            case fTeam figure of
                -- maybe redundant check of white cell, but it is safer
                Blues -> setColor $ if isWhiteCell x y then colors BlueWhite else colors BlueBlack
                Reds  -> setColor $ if isWhiteCell x y then colors RedWhite else colors RedBlack
            setAttribute AttributeBlink $ isSelected figure
            case fType figure of
                King    -> if isDebug s then
                    drawString (
                        show x ++ (if figure `elem` snd (fromMaybe (s, []) $ turnResult s) then "X" else "█") ++ show y
                    ) else drawString "███"
                Checker -> (if isDebug s then
                                (do drawString (show x)
                                    if figure
                                         `elem` snd (fromMaybe (s, []) $ turnResult s) then
                                        drawString "X"
                                    else
                                        drawGlyph glyphBoard
                                    drawString (show y))
                            else
                                (do drawGlyph glyphBoard
                                    drawGlyph glyphBoard
                                    drawGlyph glyphBoard))
            setAttribute AttributeBlink False
            setColor $ if isWhiteCell x y then colors IdleWhite else colors IdleBlack
        Nothing -> drawString $ if any isSelected (figures s) && isJust (turnResult (s {cursor = (x, y)})) then " + " else "   "
    if cursor s == (x, y)
        then drawGlyph glyphLineV
        else drawString " "
    moveCursor (fromIntegral $ (8-x) * 3 + 4) (fromIntegral $ (y-1) * 5 + 3)
    if cursor s == (x, y)
        then do
            drawGlyph glyphCornerLL
            drawGlyph glyphLineH
            drawGlyph glyphLineH
            drawGlyph glyphLineH
            drawGlyph glyphCornerLR
        else drawString "     "
    setColor $ colors IdleBlack
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

updateOptions :: Window -> (MyColor -> ColorID) -> GameState -> Curses ()
updateOptions ow colors s = do
    updateWindow ow $ drawOptions colors s
    render


drawOptions :: (MyColor -> ColorID) -> GameState -> Update ()
drawOptions colors s = do
    setColor $ colors IdleBlack
    drawBorder Nothing Nothing Nothing Nothing Nothing Nothing Nothing Nothing
    moveCursor 0 11
    setColor $ if inOptions s then colors IdleWhite else colors IdleBlack
    drawString " OPTIONS "
    setColor $ colors IdleBlack
    moveCursor 2 2
    setColor $ colors RedBlack
    drawString "██"
    setColor $ colors IdleBlack
    moveCursor 2 5
    drawTeamString s Reds
    moveCursor 4 2
    setColor $ colors BlueBlack
    drawString "██"
    setColor $ colors IdleBlack
    moveCursor 4 5
    drawTeamString s Blues
    setColor $ if option s == 0 then colors IdleWhite else colors IdleBlack
    moveCursor 6 2
    drawString "        Change Team        "
    setColor $ if option s == 1 then colors IdleWhite else colors IdleBlack
    moveCursor 8 2
    drawString " "
    drawGlyph glyphArrowL
    drawString " "
    moveCursor 8 27
    drawString " "
    drawGlyph glyphArrowR
    drawString " "
    setColor $ colors IdleBlack
    moveCursor 8 12
    drawString $ "Level: " ++ show (level s)
    setColor $ if option s == 2 then colors IdleWhite else colors IdleBlack
    moveCursor 23 2
    drawString "          New Game          "
    setColor $ if option s == 3 then colors IdleWhite else colors IdleBlack
    moveCursor 25 2
    drawString "       "
    drawGlyph $ if isDebug s then glyphBlock else glyphBullet
    drawString " Debug  Mode       "
    setColor $ colors IdleBlack


drawTeamString :: GameState -> Team -> Update ()
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

updateDebug :: Window -> GameState -> Curses ()
updateDebug dw s = do
    (_, sw) <- screenSize
    updateWindow dw clear
    when (isDebug s) $ updateWindow dw $ drawDebug s sw
    render


dummyFigure :: (Int, Int) -> Figure
dummyFigure c = Figure Blues Checker c False


drawDebug :: GameState -> Integer -> Update ()
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
    drawString $ "Cached:\t" ++ show (length $ aiCache s) -- ++ "\t" ++ show (aiCache s)
