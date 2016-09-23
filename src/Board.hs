module Board (
    Cell,
    Cursor,
    Selection,
    State(..),
    doCursorMove,
    generateFigures,
    Figure,
    figureType,
    figureTeam,
    figureCell,
    Team(..),
    FigureType(..),
    getFigureFromList,
    getFigure,
    isFigureSelected,
    getSelectedFigure
) where

import Control
import Control.Monad (guard)
import Data.Maybe (isJust, fromJust, isNothing, catMaybes)

data State = State  { cursor :: Cursor
                    , figures :: [Figure]
                    , selection :: Selection
                    }

type Cell = (Int, Int)
type Cursor = (Int, Int)
type Selection = Maybe (Int, Int)

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
                     , figureCell :: Cell
                     } deriving (Eq, Show)

generateFigures :: [Figure]
generateFigures = catMaybes $ [1..8] >>= \x -> [1..8] >>= \y -> return $ generateMaybeFigure x y

generateMaybeFigure :: Int -> Int -> Maybe Figure
generateMaybeFigure x y
    | y /= 4 && y /= 5 && mod x 2 == mod y 2 = Just Figure {
        figureType  = Checker,
        figureTeam  = if y <=3 then Blues else Reds,
        figureCell  = (x, y)
    }
    | otherwise = Nothing

getFigure :: State -> Cell -> Maybe Figure
getFigure state = getFigureFromList $ figures state

getFigureFromList :: [Figure] -> Cell -> Maybe Figure
getFigureFromList figs cell
    | null fig = Nothing
    | otherwise = Just $ head fig
    where
        fig = filter (\fg -> figureCell fg == cell) figs

isFigureSelected :: State -> Bool
isFigureSelected state
    | isNothing fig = False
    | otherwise = True
    where
        fig = maybe Nothing (getFigure state) sel
        sel = selection state

getSelectedFigure :: State -> Figure
getSelectedFigure state = fromJust $ getFigure state $ fromJust $ selection state

