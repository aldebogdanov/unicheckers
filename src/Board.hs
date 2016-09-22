module Board (
    Cursor,
    doCursorMove,
    generateFigures,
    Figure,
    figureType,
    figureTeam,
    figureX,
    figureY,
    Team(..),
    FigureType(..)
) where

import Control
import Control.Monad (guard)
import Data.Maybe (isJust, catMaybes)

type Cursor = (Int, Int)

doCursorMove :: Cursor -> Maybe Control -> Cursor
doCursorMove c ctrl
    | ctrl == Just ToUp = if snd c > 1 then (fst c, snd c - 1) else c
    | ctrl == Just ToDown = if snd c < 8 then (fst c, snd c + 1) else c
    | ctrl == Just ToRight = if fst c < 8 then (fst c + 1, snd c) else c
    | ctrl == Just ToLeft = if fst c > 1 then (fst c - 1, snd c) else c
    | otherwise = c

data FigureType = Checker | King deriving (Eq, Show)
data Team = Reds | Blues deriving (Eq, Show)

data Figure = Figure { figureType :: FigureType
                     , figureTeam :: Team
                     , figureX :: Int
                     , figureY :: Int
                     } deriving Show

generateFigures :: [Figure]
generateFigures = catMaybes $ [1..8] >>= \x -> [1..8] >>= \y -> return $ generateMaybeFigure x y

generateMaybeFigure :: Int -> Int -> Maybe Figure
generateMaybeFigure x y
    | y /= 4 && y /= 5 && mod x 2 == mod y 2 = Just Figure {
        figureType  = Checker,
        figureTeam  = if y <=3 then Blues else Reds,
        figureX     = x,
        figureY     = y
    }
    | otherwise = Nothing
