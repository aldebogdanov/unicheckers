module AI (
  handleAI
) where

import State
import Data.Maybe (catMaybes, mapMaybe)
import System.Random


generator = mkStdGen 42 -- $ getClockTime >>= (\(TOD _ pico) -> return pico)


handleAI :: State -> State
handleAI s = newState
  where
    variants         = processStates [[(s, 0, 0)]] (level s * 2)
    mx               = maximum $ map (\((_, ai, pl) : vs) -> ai - pl) variants
    bestVars         = filter (\((_, ai, pl) : vs) -> ai - pl == mx) variants
    len              = length bestVars
    (rand, _)        = randomR (0, len - 1) generator
    (newState, _, _) = last $ init $ bestVars!!rand


processStates :: [[(State, Int, Int)]] -> Int -> [[(State, Int, Int)]]
processStates acc num = case num of
    0 -> acc
    n -> processStates (acc >>= processState) (n - 1)


processState :: [(State, Int, Int)] -> [[(State, Int, Int)]]
processState ss =
    map (\(ns, eat) -> (if turn s == aiTeam s then (ns, aiEat + length eat, plEat) else (ns, aiEat, plEat + length eat)) : ss) newSs
  where
    (s, aiEat, plEat) = head ss
    figs              = getCurrentTeamFigures s
    newSs             = figs >>= processFigure s


processFigure :: State -> Figure -> [(State, [Figure])]
processFigure s f = mapMaybe (processTurn s f) [(x, y) | x <- [1 .. 8], y <- [1 .. 8]]


processTurn :: State -> Figure -> (Int, Int) -> Maybe (State, [Figure])
processTurn s f c = do
    ms <- selectFigure s f
    turnResult (setCursor ms c)
