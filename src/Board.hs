module Board (
    Cell,
    Cursor,
    State(..),
    doCursorMove,
    generateFigures,
    Team(..),
    FigureType(..)
) where

import Data.Maybe (catMaybes)
import State


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




