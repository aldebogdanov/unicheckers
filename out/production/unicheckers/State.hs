module State where

import Data.Maybe (isNothing, isJust)
import Data.Foldable (find)
import UI.NCurses
import Debug.Trace

data FigureType = Checker | King deriving (Eq, Show)

data Team = Reds | Blues deriving (Eq, Show)

type Cell = (Int, Int)
type Cursor = Cell

data Figure = Figure { fTeam :: Team
                     , fType :: FigureType
                     , fCell :: Cell
                     , isSelected :: Bool
                     } deriving (Eq, Show)

data State = State { turn :: Team
                   , cursor :: Cursor
                   , figures :: [Figure]
                   , isFixed :: Bool
                   } deriving (Show)


handleControl :: State -> Event -> State
handleControl state k = case k of
    EventCharacter ' ' -> selectCell state
    EventSpecialKey k  -> state { cursor = doCursorMove (cursor state) k }
    _                  -> state


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
selectFigure s f = do
    mf <- find (== f) $ figures s
    ms <- checkFixed s mf
    return $ ms { figures = map (\ f -> f { isSelected = f == mf }) $ figures ms }


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
    ms     <- checkFreeCursor ms
    mf     <- getSelectedFigure ms
    mfCell <- Just $ fCell mf
    mc     <- Just $ cursor ms
    return $ zip (getRangeBetween (fst mfCell) (fst mc)) (getRangeBetween (snd mfCell) (snd mc))


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
    ms    <- checkDirection ms me mf
    r ms mf md me
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
        Blues | trace (show direction) direction > 0 -> Just s
        Reds  | direction < 0                -> Just s
        _                                    -> Nothing
      where
        direction = snd (cursor s) - snd (fCell f)


checkNoFriendly :: State -> [(Int, Int)] -> Maybe State
checkNoFriendly s p
    | not $ any (\f -> fTeam f == turn s && elem (fCell f) p) (figures s) = Just s
    | otherwise                                                           = Nothing


getEaten :: State -> [(Int, Int)] -> Maybe [Figure]
getEaten s p = Just $ filter (\f -> fTeam f == nextTeam (turn s) && elem (fCell f) p) $ figures s


turnResult :: State -> Maybe (State, Int)
turnResult s = do
    mr <- checkPath s
    tr mr
  where
    tr mr | null (snd mr) && anyCanEat s = Nothing
          | otherwise                    = Just (updateState mr, length (snd mr))


updateState :: (State, [Figure]) -> State
updateState (s, es) = s { turn = newTurn, figures = newFigures, isFixed = newIsFixed }
  where
    notEatenFigs       = filter (`notElem` es) $ figures s
    newFigsDisposition = map (\f -> if isSelected f then f { fCell = cursor s } else f) notEatenFigs
    canEatAgain        = not (null es) && (canEat $ s {figures = newFigsDisposition}) && (canEat $ s { figures = newFigsDisposition })
    newIsFixed         = canEatAgain
    newTurn            = if canEatAgain then turn s else nextTurn s
    newFigures         = map (\f -> f { isSelected = newIsFixed && isSelected f, fType = determineType f }) newFigsDisposition


determineType :: Figure -> FigureType
determineType f = case (fTeam f, fst $ fCell f) of
    (Blues, 1) -> King
    (Reds, 8)  -> King
    _          -> fType f


getTeamFigures :: State -> Team -> [Figure]
getTeamFigures s t = filter (\fig -> fTeam fig == t) $ figures s


getDiagonalCells :: State -> [(Int, Int)]
getDiagonalCells s = filter (\(x, y) -> isJust $ checkDiagonal $ setCursor s (x, y)) [ (x, y) | x <- [1..8], y <- [1..8] ]


anyCanEat :: State -> Bool
anyCanEat s = any (can s) $ getTeamFigures s (turn s)
  where
    can s f = maybe False canEat (selectFigure s f)

canEat :: State -> Bool
canEat s = any (isEating . setCursor s) $ getDiagonalCells s


isEating :: State -> Bool
isEating s = case checkPath s of
   Nothing                       -> False
   Just (s', es) | not (null es) -> True
   _                             -> False


handleTurn :: State -> State
handleTurn s = case turnResult s of
    Nothing       -> s
    Just (ns, en) -> ns

---------------------------------------

--getSelection :: State -> Maybe Figure
--getSelection state = find (checkFigure $ cursor state) $ figures state
--
--
--checkFigure :: Cursor -> Figure -> Bool
--checkFigure cur figure = case (cur, figure) of
--    ((xc, yc), fig) -> fCell fig == (xc, yc)
--
--

--
--
--isTurnAllowed :: State -> Bool
--isTurnAllowed state
--    | not isDiag                                   = False
--    | not noFriendly                               = False
--    | eaten > 1                                    = False
--    | eaten == 1 && (fType fig == King || md == 2) = True
--    | eaten == 0 && anyCanEat state                = False
--    | eaten == 0 && (fType fig == King || md == 1) = True
--    | otherwise                                    = False
--  where
--    fig        = head $ filter isSelected $ figures state
--    position   = fCell fig
--    curs       = cursor state
--    isDiag     = abs (fst curs - fst position) == abs (snd curs - snd position)
--    md         = abs (fst curs - fst position)
--    noFriendly = checkNoFriendly state
--    eaten      = length $ getEaten state
--
--
--getPath :: State -> [(Int, Int)]
--getPath state = zip (getRangeBetween (fst $ cursor state) x) (getRangeBetween (snd $ cursor state) y)
--  where
--    fig = head $ filter isSelected $ figures state
--    x   = fst $ fCell fig
--    y   = snd $ fCell fig
--
--
getRangeBetween :: Int -> Int -> [Int]
getRangeBetween a b
    | abs (a - b) <= 1 = []
    | a < b = [(a + 1)..(b - 1)]
    | otherwise = [(b + 1)..(a - 1)]
--
--
--checkNoFriendly :: State -> Bool
--checkNoFriendly state = cnt == 0
--  where
--    cnt = length $ filter (\fig -> elem (fCell fig) (getPath state) && fTeam fig == turn state) $ figures state
--
--
--getEaten :: State -> [Figure]
--getEaten state = filter (\fig -> elem (fCell fig) (getPath state) && fTeam fig /= turn state) $ figures state
--
--
--currentCanEat :: State -> Bool
--currentCanEat state = any isSelected (figures state) && canEat state (head $ filter isSelected $ figures state)
--
--
--anyCanEat :: State -> Bool
--anyCanEat state = any (canEat state) currentFigures
--  where
--    currentFigures = filter (\fig -> fTeam fig == turn state) $ figures state
--
--
--canEat :: State -> Figure -> Bool
----canEat state figure = trace ("Can eat: " ++ show canEatStates) $ not (null canEatStates)
--canEat state figure = not (null canEatStates)
--  where
--    -- TODO: Refactor it!
--    state'       = state { figures = map (\fig -> fig {isSelected = fig == figure}) $ figures state }
--    cursors      = [ (x, y) | x <- [1..8], y <- [1..8] ]
--    cursors'     = filter (\(x, y) -> even (x + y)) cursors
--    states       = map (\(x, y) -> state' { cursor = (x, y) }) cursors'
--    freeStates   = filter (\s -> not (any (\fig -> fCell fig == cursor s) (figures s))) states
--    canEatStates = filter (\s -> length (getEaten s) == 1 && isAllowedDistanceToEat s) freeStates
--
--
---- TODO: Make typesafe
--isAllowedDistanceToEat :: State -> Bool
--isAllowedDistanceToEat state = fType fig == King || distance == 2
--  where
--    fig      = head $ filter isSelected $ figures state
--    position = fCell fig
--    curs     = cursor state
--    isDiag   = abs (fst curs - fst position) == abs (snd curs - snd position)
--    distance = abs (fst curs - fst position)
--
--
--determineNextTurn :: State -> Team
--determineNextTurn state
--    | null (getEaten state)                    = nextTurn state
--    | not (currentCanEat $ lookInFuture state) = nextTurn state
--    | otherwise                                = turn state


nextTurn :: State -> Team
nextTurn state = nextTeam $ turn state


nextTeam :: Team -> Team
nextTeam team = case team of
    Blues -> Reds
    Reds  -> Blues
--
--
--updateFigures :: State -> [Figure]
--updateFigures state =
--    map (\fig -> fig { isSelected = currentCanEat (lookInFuture state) && isSelected fig
--                     , fCell = if isSelected fig then cursor state else fCell fig }
--    ) $
--    filter (\fig -> fig `notElem` getEaten state) $ figures state
--
--
--lookInFuture :: State -> State
--lookInFuture state = state { figures = map (\fig -> if isSelected fig then fig { fCell = cursor state } else fig) $ filter (\fig -> fig `notElem` getEaten state) $ figures state }