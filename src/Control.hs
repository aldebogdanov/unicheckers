-----------------------------------------------------------------------------
--
-- Module      :  Control
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

module Control (
    ArrowKey(..),
    lookForKey
) where

import System.IO (
    stdin,
    hReady,
    hSetBuffering,
    BufferMode(..),
    stdout,
    hSetEcho)

import System.Exit
import Misc

data ArrowKey = UpArrow | DownArrow | LeftArrow | RightArrow deriving Eq

lookForKey :: String -> IO ArrowKey
lookForKey cx
    | cx == "\ESC[A" = return UpArrow
    | cx == "\ESC[B" = return DownArrow
    | cx == "\ESC[C" = return RightArrow
    | cx == "\ESC[D" = return LeftArrow
    | length cx == 3 = lookForKey ""
    | cx == "q" = die "Thank you!"
    | otherwise = do
        hSetEcho stdout False
        hSetBuffering stdin NoBuffering
        c <- getChar
        b <- hReady stdin
        lookForKey (cx ++ [c])

