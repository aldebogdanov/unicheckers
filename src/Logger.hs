{-# LANGUAGE NoImplicitPrelude #-}
module Logger(writeLog, writeLogUnsafe, writeLogShow) where

import Relude
import System.Log.FastLogger
import System.IO.Unsafe
import Data.Time.ISO8601
import Data.Time.Clock
import Config


{-# NOINLINE globalLogger #-}
globalLogger :: LoggerSet
globalLogger = unsafePerformIO $ newFileLoggerSet defaultBufSize $ logFile config

writeLogIO :: Text -> IO ()
writeLogIO txt = do
  utcTime <- getCurrentTime
  let timestampText = toText $ formatISO8601Millis utcTime
      logEntry     = timestampText <> " " <> txt <> "\n"
  pushLogStr globalLogger $ toLogStr logEntry

{-# NOINLINE writeLogUnsafe #-}
writeLogUnsafe :: Text -> ()
writeLogUnsafe txt = if loggingOn config then unsafePerformIO $ writeLogIO txt else ()


writeLog :: Text -> a -> a
writeLog txt expr = expr
  where
    !_ = writeLogUnsafe txt


writeLogShow :: ToText a => Text -> a -> a
writeLogShow txt expr = expr
  where
    !_ = writeLogUnsafe $ txt <> toText expr
