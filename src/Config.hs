{-# LANGUAGE NoImplicitPrelude #-}
module Config where

import Relude
import qualified Data.Configurator as C
import System.IO.Unsafe


data Conf = Conf { maxLevel :: Int
                 , threadsNum :: Int
                 , threshold :: Int
                 , loggingOn :: Bool
                 , logFile :: String
                 }

{-# NOINLINE conf #-}
conf = unsafePerformIO $ C.load [C.Optional "/etc/unicheckers.conf", C.Optional "./unicheckers.conf"]

{-# NOINLINE config #-}
config :: Conf
config = unsafePerformIO $ do
    mlv <- C.lookupDefault "5"                 conf "game.max_level" :: IO String
    tds <- C.lookupDefault "4"                 conf "game.threads_number" :: IO String
    thd <- C.lookupDefault "30000"             conf "ai.threshold" :: IO String
    lgo <- C.lookupDefault "Off"               conf "logs.logging_on" :: IO String
    lgf <- C.lookupDefault "./unicheckers.log" conf "logs.log_file" :: IO String
    return $ Conf
        (fromMaybe 5 $ readMaybe mlv)
        (fromMaybe 4 $ readMaybe tds)
        (fromMaybe 30000 $ readMaybe thd)
        (lgo == "On")
        lgf

