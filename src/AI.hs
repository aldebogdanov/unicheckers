{-# LANGUAGE NoImplicitPrelude #-}
module AI (
  handleAI
) where

import Relude
import Data.List (groupBy)
import GameState
import Logger
import Config
import System.Random
import GHC.Conc.Sync


generator :: StdGen
generator = mkStdGen 12  -- $ getClockTime >>= (\(TOD _ pico) -> return pico)


handleAI :: GameState -> GameState
handleAI s =
    newState { status    = if isStop newState then Stopped else InProcess
             , winner    = if isStop newState
                           then Just (if isWin' newState then aiTeam newState else nextTeam $ aiTeam newState)
                           else Nothing
             , inOptions = isStop newState
             }
  where
    cached           = readCache s
    vars             = calculateHistoryVariants (fst cached) ((level s * 2) - snd cached)
    !_               = writeLogUnsafe $ "VARIANTS:\n" <> showVariants vars
    maxRateMaybe     = viaNonEmpty last (sort $ map snd vars)
    bestVars         = case maxRateMaybe of
                           Just mr -> filter (\var -> snd var == mr) vars
                           Nothing -> vars
    !_               = writeLogUnsafe $ "MAXIMUM:\t" <> show maxRateMaybe
    !_               = writeLogUnsafe $ "BEST VARIANTS:\t" <> showVariants bestVars
    len              = length bestVars
    !_               = writeLogUnsafe $ "BESTVARS NUM:\t" <> show len
    (rand, _)        = randomR (0, len - 1) generator
    (bestVar, _)     = fromMaybe ([s], 0) (bestVars !!? rand)
    ns               = if isStop s then s else fromMaybe s (viaNonEmpty init bestVar >>= viaNonEmpty last)
    newState         = ns { aiCache = buildCache vars ns }


calculateHistoryVariants :: [(History, Int)] -> Int -> [(History, Int)]
calculateHistoryVariants acc num = h
  where
    h = case num of
        0                                                         -> acc
        _ | all isWin acc                                         ->
            writeLog ("(any is win) HISTORY expanded to " <> show (length acc) <> " variants") acc
        _ | length acc >= threshold config                        ->
            writeLog ("(threshold) HISTORY expanded to " <> show (length acc) <> " variants") acc
        n | not $ all isLoss acc                                  ->
            calculateHistoryVariants (forkHistoryChunks acc $ getChunkSize acc) (n - 1)
        _                                                         ->
            writeLog ("(any is loss) HISTORY expanded to " <> show (length acc) <> " variants") acc
    !_ = case num of
        0 -> writeLogUnsafe $ "HISTORY expanded to " <> show (length acc) <> " variants"
        _ -> writeLogUnsafe $ "HISTORY expanded (level " <> show num <> ") from " <> show (length acc) <> " variants"


getChunkSize :: [a] -> Int
getChunkSize l = length l `div` (threadsNum config - 1) + 1


forkHistoryChunks :: [(History, Int)] -> Int -> [(History, Int)]
forkHistoryChunks [] _ = []
forkHistoryChunks hs n = par hs1 (hs1 ++ hs2)
  where
    hs1 = take n hs >>= forkHistory
    hs2 = forkHistoryChunks (drop n hs) n


isStop :: GameState -> Bool
isStop s = isWin' s || isLoss' s

isWin' :: GameState -> Bool
isWin' s = null $ getTeamFigures s $ nextTeam (aiTeam s)

isWin :: (History, Int) -> Bool
isWin hi = maybe False isWin' $ viaNonEmpty head $ fst hi

isLoss' :: GameState -> Bool
isLoss' s = null $ getTeamFigures s $ aiTeam s

isLoss :: (History, Int) -> Bool
isLoss hi = maybe False isLoss' $ viaNonEmpty head $ fst hi


forkHistory :: (History, Int) -> [(History, Int)]
forkHistory h = hs
  where
    h' = fst h
    s  = viaNonEmpty head h'
    hs = case s of
        Nothing -> []
        Just s' -> map (rate . (: h')) $ getCurrentTeamFigures s' >>= processFigure s'


processFigure :: GameState -> Figure -> [GameState]
processFigure s f = mapMaybe (processTurn s f) [(x, y) | x <- [1 .. 8], y <- [1 .. 8]]


processTurn :: GameState -> Figure -> (Int, Int) -> Maybe GameState
processTurn s f c = do
    ms      <- selectFigure s f
    (ns, _) <- turnResult (setCursor ms c)
    return ns


rate :: History -> (History, Int)
rate h = case do
    s     <- viaNonEmpty head h
    t     <- Just $ aiTeam s
    aiFs  <- Just $ getTeamFigures s t
    plFs  <- Just $ getTeamFigures s $ nextTeam t
    aiFsn <- Just $ length aiFs
    plFsn <- Just $ length plFs
    aiKsn <- Just $ kingsN aiFs
    plKsn <- Just $ kingsN plFs
    return (aiFsn - plFsn + (aiKsn - plKsn) * 2 + (if plFsn == 0 then 100 else 0) - (if aiFsn == 0 then 100 else 0))
  of
    Nothing -> ([], 0)
    Just r  -> (h, r)


kingsN :: [Figure] -> Int
kingsN fs = length $ filter (\f -> fType f == King) fs


buildCache :: [(History, Int)] -> GameState -> [History]
buildCache vars ns =
    writeLog ("CACHE builded: " <> show (length cache) <> " variants of depth " <>
                  show (maybe 0 length (viaNonEmpty head cache))
             ) cache
  where
    cache = mapMaybe (\(var, _) -> case viaNonEmpty init var >>= viaNonEmpty last of
                                       Just turnDone | compareTurns turnDone ns
                                           -> Just (take (length var - (if isFixed ns then 1 else 2)) var)
                                       _   -> Nothing
                     ) vars


readCache :: GameState -> ([(History, Int)], Int)
readCache s = writeLog ("CACHE readed: " <> show (length cached) <> " variants of depth " <>
                            show (case viaNonEmpty head cached of
                                      Nothing        -> 0
                                      Just cacheItem -> length (fst cacheItem))
                       )$
              case viaNonEmpty head cached of
                  Nothing -> ([([s], 0)], 0)
                  Just hc -> (cached, length $ fst hc)
            where
              cached = mapMaybe (\var -> case viaNonEmpty last var of
                                             Just lastState | compareTurns lastState s -> Just (var, 0)
                                             _                                         -> Nothing


                                ) $ aiCache s


compareTurns :: GameState -> GameState -> Bool
compareTurns s1 s2 = figures s1 == figures s2 && turn s1 == turn s2


showVariants :: [([GameState], Int)] -> Text
showVariants vs = if not $ loggingOn config then "" else toText $ intercalate ", " $ map (\gr -> show (snd gr) ++ ": " ++ show (fst gr)) ratedGroups
  where
    cmp a b     = compare (snd a) (snd b)
    eq a b      = snd a == snd b
    groups      = groupBy eq (sortBy cmp vs)
    ratedGroups = map (\gr -> (length gr, case viaNonEmpty head gr of
                                              Just (_, r)    -> r
                                              Nothing        -> 0)
                      ) groups