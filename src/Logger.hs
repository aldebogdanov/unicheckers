module Logger(writeLog, writeLogUnsafe, writeLogShow) where

import System.Log.FastLogger
import System.IO.Unsafe
import Data.Time.ISO8601
import Data.Time.Clock
import Config


{-# NOINLINE globalLogger #-}
globalLogger :: LoggerSet
globalLogger = unsafePerformIO $ newFileLoggerSet defaultBufSize $ logFile config

writeLogIO :: String -> IO ()
writeLogIO str = do
  utcTime <- getCurrentTime
  let timestampStr = formatISO8601Millis utcTime
      logEntry     = timestampStr ++ " " ++ str ++ "\n"
  pushLogStr globalLogger $ toLogStr logEntry

{-# NOINLINE writeLogUnsafe #-}
writeLogUnsafe :: String -> ()
writeLogUnsafe str = if loggingOn config then unsafePerformIO $ writeLogIO str else ()


writeLog :: String -> a -> a
writeLog str expr = expr
  where
    !_ = writeLogUnsafe str


writeLogShow :: Show a => String -> a -> a
writeLogShow str expr = expr
  where
    !_ = writeLogUnsafe $ str ++ show expr
