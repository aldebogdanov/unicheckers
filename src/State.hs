module State where

import Data.Maybe (isJust, catMaybes)
import Data.Foldable (find)
import UI.NCurses (Event(EventCharacter, EventSpecialKey), Key(..))

data FigureType = Checker | King deriving (Eq, Show)

data Team = Reds | Blues deriving (Eq, Show)

data GameStatus = Stopped | InProcess deriving (Eq, Show)

type Cell = (Int, Int)
type Cursor = Cell

data Figure = Figure { fTeam :: Team
                     , fType :: FigureType
                     , fCell :: Cell
                     , isSelected :: Bool
                     } deriving (Eq, Show)

data State = State { status    :: GameStatus
                   , lastVar   :: [(State, Int, Int)]
                   , turn      :: Team
                   , cursor    :: Cursor
                   , figures   :: [Figure]
                   , isFixed   :: Bool
                   , aiTeam    :: Team
                   , level     :: Int
                   , winner    :: Maybe Team
                   , inOptions :: Bool
                   , option    :: Int
                   , isDebug   :: Bool
                   , variants  :: [([State], Int)]
                   }

instance Show State where
    show (State st _ t _ fs _ _ _ _ _ _ _ vs) = "<" ++ show st ++ ", " ++ show t ++ ", " ++ show (length fs) ++ " figures, " ++ show (length vs) ++ " variants>"


handleOptionsControl :: State -> Event -> State
handleOptionsControl s e = case e of
    EventCharacter ' '         -> case option s of
                                      0 -> s { aiTeam = nextTeam $ aiTeam s }
                                      2 -> s { status = InProcess
                                             , turn = Blues
                                             , winner = Nothing
                                             , figures = generateFigures
                                             , inOptions = False
                                             }
                                      3 -> s { isDebug = not (isDebug s) }
                                      _ -> s
    EventSpecialKey KeyUpArrow    | option s > 0 -> s { option = option s - 1 }
    EventSpecialKey KeyDownArrow  | option s < 3 -> s { option = option s + 1 }

    EventSpecialKey KeyLeftArrow  | option s == 1 && level s > 1 -> s { level = level s - 1 }
    EventSpecialKey KeyRightArrow | option s == 1 && level s < 7 -> s { level = level s + 1 }
    _ -> s


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


handleGameControl :: State -> Event -> State
handleGameControl s e = case e of
    EventCharacter ' ' -> selectCell s
    EventSpecialKey k  -> s { cursor = doCursorMove (cursor s) k }
    _                  -> s


doCursorMove :: Cursor -> Key -> Cursor
doCursorMove c k = case k of
    KeyUpArrow    -> if fst c < 8 then (fst c + 1, snd c) else c
    KeyDownArrow  -> if fst c > 1 then (fst c - 1, snd c) else c
    KeyRightArrow -> if snd c < 8 then (fst c, snd c + 1) else c
    KeyLeftArrow  -> if snd c > 1 then (fst c, snd c - 1) else c
    _             -> c


selectCell :: State -> State
selectCell s = case getFigure s of
    Just f | not (isFixed s) && fTeam f == turn s && isSelected f -> s { figures = map (\fig -> fig { isSelected = False }) $ figures s }
    Just f | not (isFixed s) && fTeam f == turn s                 -> s { figures = map (\fig -> fig { isSelected = fCell fig == cursor s }) $ figures s }
    Just _                                                        -> s
    Nothing -> case getSelectedFigure s of
        Just _  -> handleTurn s
        Nothing -> s


setCursor :: State -> (Int, Int) -> State
setCursor s c = s { cursor = c }


selectFigure :: State -> Figure -> Maybe State
selectFigure s fig = do
    mf <- find (== fig) $ figures s
    ms <- checkFixed s mf
    return $ ms { figures = map (\f -> f { isSelected = f == mf }) $ figures ms }


checkFixed :: State -> Figure -> Maybe State
checkFixed s f = if isFixed s && another then Nothing else Just s
  where
    another = case getSelectedFigure s of
        Nothing  -> False
        Just fig -> fig /= f


getFigure :: State -> Maybe Figure
getFigure s = find (\f -> fCell f == cursor s) $ figures s


getSelectedFigure :: State -> Maybe Figure
getSelectedFigure s = find isSelected $ figures s


checkFreeCursor :: State -> Maybe State
checkFreeCursor s = if not $ any (\f -> fCell f == cursor s) $ figures s then Just s else Nothing


checkDiagonal :: State -> Maybe State
checkDiagonal s = do
    mf <- getSelectedFigure s
    if abs (fst (fCell mf) - fst (cursor s)) == abs (snd (fCell mf) - snd (cursor s)) then Just s else Nothing


getPath :: State -> Maybe [(Int, Int)]
getPath s = do
    ms     <- checkDiagonal s
    ms'    <- checkFreeCursor ms
    mf     <- getSelectedFigure ms'
    mfCell <- Just $ fCell mf
    mc     <- Just $ cursor ms'
    return $ filter (`notElem` [mc, mfCell]) $ fullPath mfCell mc


fullPath :: (Int, Int) -> (Int, Int) -> [(Int, Int)]
fullPath mfCell c = 
    zip [xStart, xThen .. xEnd] [yStart, yThen .. yEnd]
  where
    xStart = fst mfCell
    xEnd   = fst c
    yStart = snd mfCell
    yEnd   = snd c
    xThen  = if xStart > xEnd then xStart - 1 else xStart + 1
    yThen  = if yStart > yEnd then yStart - 1 else yStart + 1


getDistance :: State -> Maybe Int
getDistance s = do
    mp <- getPath s
    return $ length mp + 1


checkPath :: State -> Maybe (State, [Figure])
checkPath s = do
    mp    <- getPath s
    mf    <- getSelectedFigure s
    md    <- getDistance s
    ms    <- checkNoFriendly s mp
    me    <- getEaten ms mp
    ms'   <- checkDirection ms me mf
    r ms' mf md me
  where
    r ms mf md me | length me > 1                                    = Nothing
                  | null me && md /= 1 && fType mf == Checker        = Nothing
                  | length me == 1 && md /= 2 && fType mf == Checker = Nothing
                  | otherwise                                        = Just (ms, me)


checkDirection :: State -> [Figure] -> Figure -> Maybe State
checkDirection s es f = case fType f of
    King                    -> Just s
    Checker | not (null es) -> Just s
    Checker                 -> case fTeam f of
        Blues | direction > 0 -> Just s
        Reds  | direction < 0 -> Just s
        _                     -> Nothing
      where
        direction = fst (cursor s) - fst (fCell f)


checkNoFriendly :: State -> [(Int, Int)] -> Maybe State
checkNoFriendly s p
    | not $ any (\f -> fTeam f == turn s && elem (fCell f) p) (figures s) = Just s
    | otherwise                                                           = Nothing


getEaten :: State -> [(Int, Int)] -> Maybe [Figure]
getEaten s p = Just $ filter (\f -> fTeam f == nextTeam (turn s) && elem (fCell f) p) $ figures s


turnResult :: State -> Maybe (State, [Figure])
turnResult s = do
    mr <- checkPath s
    tr mr
  where
    tr mr | null (snd mr) && anyCanEat s = Nothing
          | otherwise                    = Just (updateState mr, snd mr)


updateState :: (State, [Figure]) -> State
updateState (s, es) = s { turn = newTurn, figures = newFigures, isFixed = newIsFixed }
  where
    notEatenFigs       = filter (`notElem` es) $ figures s
    newFigsDisposition = map (\f -> if isSelected f then f { fCell = cursor s } else f) notEatenFigs
    canEatAgain        = not (null es) && (canEat $ s {figures = newFigsDisposition}) && (canEat $ s { figures = newFigsDisposition })
    newIsFixed         = canEatAgain
    newTurn            = if canEatAgain then turn s else nextTurnTeam s
    newFigures         = map (\f -> f { isSelected = newIsFixed && isSelected f, fType = determineType f }) newFigsDisposition


determineType :: Figure -> FigureType
determineType f = case (fTeam f, fst $ fCell f) of
    (Blues, 8) -> King
    (Reds, 1)  -> King
    _          -> fType f


getTeamFigures :: State -> Team -> [Figure]
getTeamFigures s t = filter (\fig -> fTeam fig == t) $ figures s


getCurrentTeamFigures :: State -> [Figure]
getCurrentTeamFigures s = getTeamFigures s (turn s)


getDiagonalCells :: State -> [(Int, Int)]
getDiagonalCells s = filter (\(x, y) -> isJust $ checkDiagonal $ setCursor s (x, y)) [ (x, y) | x <- [1..8], y <- [1..8] ]


anyCanEat :: State -> Bool
anyCanEat s = any (can s) $ getCurrentTeamFigures s
  where
    can s' f = maybe False canEat (selectFigure s' f)


canEat :: State -> Bool
canEat s = any (isEating . setCursor s) $ getDiagonalCells s


isEating :: State -> Bool
isEating s = case checkPath s of
   Nothing                      -> False
   Just (_, es) | not (null es) -> True
   _                            -> False


handleTurn :: State -> State
handleTurn s = case turnResult s of
    Nothing       -> s
    Just (ns, _) -> ns


getRangeBetween :: Int -> Int -> [Int]
getRangeBetween a b
    | abs (a - b) <= 1 = []
    | a < b = [(a + 1)..(b - 1)]
    | otherwise = [(a - 1)..(b + 1)]


nextTurnTeam :: State -> Team
nextTurnTeam state = nextTeam $ turn state


nextTeam :: Team -> Team
nextTeam team = case team of
    Blues -> Reds
    Reds  -> Blues
