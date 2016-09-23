module Main (
    main
) where

import AI
import Board
import Draw
import Control
import System.Exit (die)
import Data.Maybe (isNothing)

main :: IO ()
main = do
    let initial = State {
                      cursor = (3, 3)
                    , figures = generateFigures
                    , selection = Nothing
                    }
    loop initial
        where
            loop state = do
                drawBoard state
                ctrl <- lookForCtrl
                case ctrl of
                    Nothing -> loop state
                    Just Exit -> die "Thank you!"
                    _ -> loop $ handleControl state ctrl

handleControl :: State -> Maybe Control -> State
handleControl state ctrl
    | ctrl `elem` map Just directions = state { cursor = doCursorMove (cursor state) ctrl }
    | ctrl == Just Select = selectCell state
    | isNothing ctrl = state

selectCell :: State -> State
selectCell state
    | Just (cursor state) == selection state = state { selection = Nothing }
    | isFigureSelected state = moveFigure state
    | otherwise = state { selection = Just (cursor state) }



