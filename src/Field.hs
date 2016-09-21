-----------------------------------------------------------------------------
--
-- Module      :  Field
-- Copyright   :
-- License     :  MIT
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Field (
    Cursor,
    doCursorMove
) where

import Control

type Cursor = (Int, Int)

doCursorMove :: Cursor -> ArrowKey -> Cursor
doCursorMove c key
    | key == UpArrow = if snd c > 1 then (fst c, snd c - 1) else c
    | key == DownArrow = if snd c < 8 then (fst c, snd c + 1) else c
    | key == RightArrow = if fst c < 8 then (fst c + 1, snd c) else c
    | key == LeftArrow = if fst c > 1 then (fst c - 1, snd c) else c
    | otherwise = c
