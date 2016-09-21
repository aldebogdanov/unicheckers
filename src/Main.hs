module Main (
    main
) where

import Board
import Field
import Control

main :: IO ()
main = do
    let c' = (3,3)
    loop c'
        where
            loop c = do
                drawBoard c
                key <- lookForKey ""
                loop $ doCursorMove c key
