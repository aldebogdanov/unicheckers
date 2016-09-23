module AI (
    baseCheckerMoves,
    moveFigure,
    getRangeBetween,
    Move
) where

import Board
import Data.Maybe (isNothing, fromJust)

--figureMoves :: State -> Figure -> [(Int, Int)]
--figureMoves state fig
--    | figureType fig == Checker = checkerMoves state fig
--    | othrwise = []

--checkerMoves :: State -> Figure -> [(Int, Int)]
--checkerMoves state fig = do
--    baseMoves <- baseCheckerMoves


baseCheckerMoves :: [(Int -> Int, Int -> Int)]
baseCheckerMoves = [(+1),(`subtract` 1)] >>= \f -> [(+1),(`subtract` 1)] >>= \g -> return (f, g)

move :: Cell -> (Int -> Int, Int -> Int) -> Cell
move (x, y) (f, g) = (f x, g y)


moveFigure :: State -> State
moveFigure state =
    state   { figures = filter (`notElem` eatenFigs) (figures newState) }
    where
        eatenFigs = getEatenFigures state move
        move = getMove state
        newState = state    { figures = map (
                                \fig -> if figureCell fig == source move
                                        then fig { figureCell = dest move }
                                        else fig
                                ) (figures state)
                            }

data Move = Move    { figure    :: Figure
                    , source    :: Cell
                    , dest      :: Cell
                    }

getMove :: State -> Move
getMove state = Move    { figure    = getSelectedFigure state
                        , source    = fromJust (selection state)
                        , dest      = cursor state
                        }

getEatenFigures :: State -> Move -> [Figure]
getEatenFigures state move = do
    let cells = cellsBetween (source move) (dest move)
    foldr (\cell figs -> maybe figs (:figs) (maybeEnemyFigure state (figureTeam (figure move)) cell)) [] cells

maybeEnemyFigure :: State -> Team -> Cell -> Maybe Figure
maybeEnemyFigure state team cell
    | isNothing fig = Nothing
    | figureTeam (fromJust fig) /= team = fig
    | otherwise = Nothing
    where
        fig = getFigure state cell

cellsBetween :: Cell -> Cell -> [Cell]
cellsBetween a b = zip r1 r2
    where
        r1 = getRangeBetween (fst a) (fst b)
        r2 = getRangeBetween (snd a) (snd b)

getRangeBetween :: Int -> Int -> [Int]
getRangeBetween a b
    | a < b = [(a + 1)..(b - 1)]
    | otherwise = [(b + 1)..(a - 1)]
