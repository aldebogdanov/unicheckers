module AI (
  handleAI
) where

import State
import Data.Maybe (catMaybes, mapMaybe)
import System.Random
import Control.Concurrent


generator = mkStdGen 12  -- $ getClockTime >>= (\(TOD _ pico) -> return pico)


handleAI :: State -> State
handleAI s = newState
  where
    variants         = processStates [[(s, 0, 0)]] (level s * 2 - 1)
    mx               = maximum $ map (\((_, ai, pl) : vs) -> ai - pl) variants
    bestVars         = filter (\((_, ai, pl) : vs) -> ai - pl == mx) variants
    len              = length bestVars
    (rand, _)        = randomR (0, len - 1) generator
    bestVar          = if len > 0 then init $ bestVars!!rand else init $ lastVar s
    (ns1, _, _)      = last bestVar
    (ns2, _, _)      = if checkWin ns1
                       then (ns1 { status = Stopped, winner = Just $ turn s, inOptions = True }, 0, 0)
                       else (ns1, 0, 0)
    (newState, _, _) = (ns2 { lastVar = bestVar, variants = variants }, 0, 0)


checkWin :: State -> Bool
checkWin s = null (getTeamFigures s Blues) || null (getTeamFigures s Reds)


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
