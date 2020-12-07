module Config where

import qualified Data.Configurator as C
import qualified Data.Text as T
import System.IO.Unsafe


data Conf = Conf { maxLevel :: Int
                 , threadsNum :: Int
                 , threshold :: Int
                 , filterOn :: Bool
                 , loggingOn :: Bool
                 , logFile :: String
                 }

{-# NOINLINE conf #-}
conf = unsafePerformIO $ C.load [C.Optional "/etc/unicheckers.conf", C.Optional "./unicheckers.conf"]

{-# NOINLINE config #-}
config :: Conf
config = unsafePerformIO $ do
    mlv <- C.lookupDefault "5"                 conf (T.pack "game.max_level")
    tds <- C.lookupDefault "4"                 conf (T.pack "game.threads_number")
    thd <- C.lookupDefault "30000"             conf (T.pack "ai.threshold")
    flt <- C.lookupDefault "On"                conf (T.pack "ai.filter")
    lgo <- C.lookupDefault "Off"               conf (T.pack "logs.logging_on")
    lgf <- C.lookupDefault "./unicheckers.log" conf (T.pack "logs.log_file")
    return $ Conf
        (read mlv :: Int)
        (read tds :: Int)
        (read thd :: Int)
        (flt == "On")
        (lgo == "On")
        lgf

