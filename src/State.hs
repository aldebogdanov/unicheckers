module State where

import Data.Maybe (isNothing)
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
selectCell state = case getSelection state of
    Just figure | (fTeam figure == turn state) && isSelected figure -> state { figures = map (\fig -> fig { isSelected = False }) $ figures state }
    Just figure | fTeam figure == turn state                        -> state { figures = map (\fig -> fig { isSelected = fCell fig == cursor state }) $ figures state }
    Just _                                                          -> state
    Nothing -> case find (\fig -> isSelected fig && fTeam fig == turn state) $ figures state of
        Just _  -> handleTurn state
        Nothing -> state


getSelection :: State -> Maybe Figure
getSelection state = find (checkFigure $ cursor state) $ figures state


checkFigure :: Cursor -> Figure -> Bool
checkFigure cur figure = case (cur, figure) of
    ((xc, yc), fig) -> fCell fig == (xc, yc)


handleTurn :: State -> State
handleTurn state
    | not $ isTurnAllowed state = state
    | otherwise                 = state { turn = determineNextTurn state
                                        , figures = updateFigures state
                                        }


isTurnAllowed :: State -> Bool
isTurnAllowed state
    | not isDiag                                   = False
    | not noFriendly                               = False
    | eaten > 1                                    = False
    | eaten == 1 && (fType fig == King || md == 2) = True
    | eaten == 0 && anyCanEat state                = False
    | eaten == 0 && (fType fig == King || md == 1) = True
    | otherwise                                    = False
  where
    fig        = head $ filter isSelected $ figures state
    position   = fCell fig
    curs       = cursor state
    isDiag     = abs (fst curs - fst position) == abs (snd curs - snd position)
    md         = abs (fst curs - fst position)
    noFriendly = checkNoFriendly state
    eaten      = length $ getEaten state


getPath :: State -> [(Int, Int)]
getPath state = zip (getRangeBetween (fst $ cursor state) x) (getRangeBetween (snd $ cursor state) y)
  where
    fig = head $ filter isSelected $ figures state
    x   = fst $ fCell fig
    y   = snd $ fCell fig


getRangeBetween :: Int -> Int -> [Int]
getRangeBetween a b
    | abs (a - b) == 1 = []
    | a < b = [(a + 1)..(b - 1)]
    | otherwise = [(b + 1)..(a - 1)]


checkNoFriendly :: State -> Bool
checkNoFriendly state = cnt == 0
  where
    cnt = length $ filter (\fig -> elem (fCell fig) (getPath state) && fTeam fig == turn state) $ figures state


getEaten :: State -> [Figure]
getEaten state = filter (\fig -> elem (fCell fig) (getPath state) && fTeam fig /= turn state) $ figures state


currentCanEat :: State -> Bool
currentCanEat state = any isSelected (figures state) && canEat state (head $ filter isSelected $ figures state)


anyCanEat :: State -> Bool
anyCanEat state = any (canEat state) currentFigures
  where
    currentFigures = filter (\fig -> fTeam fig == turn state) $ figures state


canEat :: State -> Figure -> Bool
--canEat state figure = trace ("Can eat: " ++ show canEatStates) $ not (null canEatStates)
canEat state figure = not (null canEatStates)
  where
    -- TODO: Refactor it!
    state'       = state { figures = map (\fig -> fig {isSelected = fig == figure}) $ figures state }
    cursors      = [ (x, y) | x <- [1..8], y <- [1..8] ]
    cursors'     = filter (\(x, y) -> even (x + y)) cursors
    states       = map (\(x, y) -> state' { cursor = (x, y) }) cursors'
    freeStates   = filter (\s -> not (any (\fig -> fCell fig == cursor s) (figures s))) states
    canEatStates = filter (\s -> length (getEaten s) == 1 && isAllowedDistanceToEat s) freeStates


-- TODO: Make typesafe
isAllowedDistanceToEat :: State -> Bool
isAllowedDistanceToEat state = fType fig == King || distance == 2 
  where
    fig      = head $ filter isSelected $ figures state
    position = fCell fig
    curs     = cursor state
    isDiag   = abs (fst curs - fst position) == abs (snd curs - snd position)
    distance = abs (fst curs - fst position)


determineNextTurn :: State -> Team
determineNextTurn state
    | null (getEaten state)                    = nextTurn state
    | not (currentCanEat $ lookInFuture state) = nextTurn state
    | otherwise                                = turn state


nextTurn :: State -> Team
nextTurn state = nextTeam $ turn state


nextTeam :: Team -> Team
nextTeam team = case team of
    Blues -> Reds
    Reds  -> Blues


updateFigures :: State -> [Figure]
updateFigures state =
    map (\fig -> fig { isSelected = currentCanEat (lookInFuture state) && isSelected fig
                     , fCell = if isSelected fig then cursor state else fCell fig }
    ) $ 
    filter (\fig -> fig `notElem` getEaten state) $ figures state


lookInFuture :: State -> State
lookInFuture state = state { figures = map (\fig -> if isSelected fig then fig { fCell = cursor state } else fig) $ filter (\fig -> fig `notElem` getEaten state) $ figures state }