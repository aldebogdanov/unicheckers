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
    Control(..),
    lookForCtrl,
    directions
) where

import System.IO (
    stdin,
    hReady,
    hSetBuffering,
    BufferMode(..),
    stdout,
    hSetEcho)
import Misc

data Control = ToUp | ToDown | ToLeft | ToRight | Exit | Select deriving Eq
directions = [ToUp, ToDown, ToLeft, ToRight]

lookForCtrl :: IO (Maybe Control)
lookForCtrl = do
    hSetEcho stdout False
    hSetBuffering stdin NoBuffering
    c <- getChar
    b <- hReady stdin
    lookChar c b
        where
            lookChar c b
                | c == '\ESC' && not b = return $ Just Exit
                | c == ' ' = return $ Just Select
                | c == '\ESC' = do
                    c <- getChar
                    if c /= '[' then return Nothing
                    else do
                        c <- getChar
                        return $ specialChars c
                | otherwise = return Nothing

specialChars :: Char -> Maybe Control
specialChars 'A' = Just ToUp
specialChars 'B' = Just ToDown
specialChars 'C' = Just ToRight
specialChars 'D' = Just ToLeft
specialChars _ = Nothing





















