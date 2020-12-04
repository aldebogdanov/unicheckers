module AI (
  handleAI
) where

import State
import Data.Maybe (catMaybes)
import System.Random
import Data.Time.Clock.POSIX (getPOSIXTime)


generator = mkStdGen getPOSIXTime


handleAI :: State -> State
handleAI s = newState
  where
    variants         = processStates [[s, 0, 0]] ((level s) * 2)
    mx               = maximum $ map (\(_, ai, pl) : vs -> ai - pl) variants
    bestVars         = filter (\(_, ai, pl) : vs -> ai - pl == mx) variants
    len              = length bestVars
    (rand, _)        = randomR (0, (len - 1)) generator
    (newState, _, _) = last $ init $ bestVars!!rand


processStates :: [[(State, Int, Int)]] -> Int -> [[(State, Int, Int)]]
processStates acc num = case num of
    0 -> acc
    n -> processStates (acc >>= processState) (n - 1)


processState :: [(State, Int, Int)] -> [[(State, Int, Int)]]
processState ss =
    map (\ns -> (if team s == aiTeam s then (ns, aiEat + (snd ns), plEat) else (ns, aiEat, plEat + (snd ns))) : ss) newSs
  where
    (s, aiEat, plEat) = head ss
    figs              = getCurrentTeamFigures s
    newSs             = figs >>= (processFigure s)


processFigure :: State -> Figure -> [(State, Int)]
processFigure s f = catMaybes $ map (processTurn s f) [ (x, y) | x <- [1..8], y <- [1..8] ]


processTurn :: State -> Figure -> (Int, Int) -> Maybe (State, Int)
processTurn s f c = do
    ms <- selectFigure s f
    ms <- setCursor ms c
    return $ turnResult ms
