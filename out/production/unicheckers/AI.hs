module AI (
--    moveFigure
) where

--import State
--import Board
--import Data.Maybe (isNothing, fromJust)

--figureMoves :: State -> Figure -> [(Int, Int)]
--figureMoves state fig
--    | figureType fig == Checker = checkerMoves state fig
--    | othrwise = []

--checkerMoves :: State -> Figure -> [(Int, Int)]
--checkerMoves state fig = do
--    baseMoves <- baseCheckerMoves


--baseCheckerMoves :: [(Int -> Int, Int -> Int)]
--baseCheckerMoves = [(+1),(`subtract` 1)] >>= \f -> [(+1),(`subtract` 1)] >>= \g -> return (f, g)
--
--move :: Cell -> (Int -> Int, Int -> Int) -> Cell
--move (x, y) (f, g) = (f x, g y)
--
--
--moveFigure :: State -> State
--moveFigure state =
--    state   { figures = filter (`notElem` eatenFigs) (figures newState) }
--    where
--        eatenFigs = getEatenFigures state move
--        move = getMove state
--        newState = state    { figures = map (
--                                \fig -> if figureCell fig == source move
--                                        then fig { fCell = dest move }
--                                        else fig
--                                ) (figures state)
--                            }
--
--data Move = Move    { figure    :: Figure
--                    , source    :: Cell
--                    , dest      :: Cell
--                    }
--
--
--getMove :: State -> Move
--getMove state = Move    { figure    = fin $ figures state
--                        , source    = fromJust (selection state)
--                        , dest      = cursor state
--                        }
--
--
--getRangeBetween :: Int -> Int -> [Int]
--getRangeBetween a b
--    | a < b = [(a + 1)..(b - 1)]
--    | otherwise = [(b + 1)..(a - 1)]
