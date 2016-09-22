module Main (
    main
) where

import Board
import Draw
import Control
import Data.Maybe (isNothing)
import System.Exit (die)

main :: IO ()
main = do
    let c' = (3,3)
    loop c'
        where
            loop c = do
                drawBoard c generateFigures
                ctrl <- lookForCtrl
                case ctrl of
                    Nothing -> loop c
                    Just Exit -> die "Thank you!"
                    _ -> loop $ handleControl c ctrl

handleControl :: Cursor -> Maybe Control -> Cursor
handleControl c ctrl
    | ctrl `elem` map Just directions = doCursorMove c ctrl
    | ctrl == Just Select = c
    | isNothing ctrl = c
