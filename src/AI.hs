module AI (
  handleAI
) where

import State
import Logger
import Config
import Data.Maybe (mapMaybe)
import System.Random
import GHC.Conc.Sync
import Data.List


generator :: StdGen
generator = mkStdGen 12  -- $ getClockTime >>= (\(TOD _ pico) -> return pico)


handleAI :: State -> State
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
    maxRate          = writeLogShow "MAXIMUM:\t" $ maximum $ map snd vars -- Exception returned on empty list? Rly?
    !_               = writeLogUnsafe $ "VARIANTS:\n" ++ showVariants vars
    bestVars         = filter (\var -> snd var == maxRate) vars
    !_               = writeLogUnsafe $ "BEST VARIANTS:\n" ++ showVariants bestVars
    len              = writeLogShow "BESTVARS NUM:\t" $ length bestVars
    (rand, _)        = randomR (0, len - 1) generator
    (bestVar, _)     = if null bestVars then ([s], 0) else bestVars!!rand 
    ns               = if isStop s then s else last $ init bestVar
    newState         = ns { aiCache = buildCache vars ns }


calculateHistoryVariants :: [(History, Int)] -> Int -> [(History, Int)]
calculateHistoryVariants acc num = h
  where
    h = case num of
        0                                                         -> acc
        _ | all isWin acc                                         ->
            writeLog ("(any is win) HISTORY expanded to " ++ show (length acc) ++ " variants") acc
        _ | length acc >= threshold config                        ->
            writeLog ("(threshold) HISTORY expanded to " ++ show (length acc) ++ " variants") acc
        n | not $ all isLoss acc                                  ->
            calculateHistoryVariants (forkHistoryChunks (filteredVariants acc) $ getChunkSize acc) (n - 1)
        _                                                         ->
            writeLog ("(any is loss) HISTORY expanded to " ++ show (length acc) ++ " variants") acc
    !_ = case num of
        0 -> writeLogUnsafe $ "HISTORY expanded to " ++ show (length acc) ++ " variants"
        _ -> writeLogUnsafe $ "HISTORY expanded (level " ++ show num ++ ") from " ++ show (length acc) ++ " variants"

filteredVariants :: [(History, Int)] -> [(History, Int)]
filteredVariants vars = if any (\var -> snd var > 0) vars then filter (\var -> snd var > (maxRate `div` 2)) vars else vars
  where
    maxRate = maximum $ map snd vars


getChunkSize :: [a] -> Int
getChunkSize l = length l `div` (threadsNum config - 1) + 1


forkHistoryChunks :: [(History, Int)] -> Int -> [(History, Int)]
forkHistoryChunks [] _ = []
forkHistoryChunks hs n = par hs1 (hs1 ++ hs2)
  where
    hs1 = take n hs >>= forkHistory
    hs2 = forkHistoryChunks (drop n hs) n


isStop :: State -> Bool
isStop s = isWin' s || isLoss' s

isWin' :: State -> Bool
isWin' s = null $ getTeamFigures s $ nextTeam (aiTeam s)

isWin :: (History, Int) -> Bool
isWin hi = isWin' $ head $ fst hi

isLoss' :: State -> Bool
isLoss' s = null $ getTeamFigures s $ aiTeam s

isLoss :: (History, Int) -> Bool
isLoss hi = isLoss' $ head $ fst hi


forkHistory :: (History, Int) -> [(History, Int)]
forkHistory h = hs
  where
    h' = fst h
    hs = map (rate . (: h')) $ getCurrentTeamFigures (head h') >>= processFigure (head h')


processFigure :: State -> Figure -> [State]
processFigure s f = mapMaybe (processTurn s f) [(x, y) | x <- [1 .. 8], y <- [1 .. 8]]


processTurn :: State -> Figure -> (Int, Int) -> Maybe State
processTurn s f c = do
    ms      <- selectFigure s f
    (ns, _) <- turnResult (setCursor ms c)
    return ns


rate :: History -> (History, Int)
rate h =
    (h, aiFsn - plFsn + (aiKsn - plKsn) * 2 + (if plFsn == 0 then 100 else 0) - (if aiFsn == 0 then 100 else 0))
  where
    s     = head h
    t     = aiTeam s
    aiFs  = getTeamFigures s t
    plFs  = getTeamFigures s $ nextTeam t
    aiFsn = length aiFs
    plFsn = length plFs
    aiKsn = kingsN aiFs
    plKsn = kingsN plFs


kingsN :: [Figure] -> Int
kingsN fs = length $ filter (\f -> fType f == King) fs


buildCache :: [(History, Int)] -> State -> [History]
buildCache vars ns =
    writeLog ("CACHE builded: " ++ show (length cache) ++ " variants of depth " ++
                  show (if null cache then 0 else length $ head cache)
             ) cache
  where
    cache = mapMaybe (\(var, _) -> if length var > 1 && compareTurns (last $ init var) ns
                                   then Just (take (length var - (if isFixed ns then 1 else 2)) var)
                                   else Nothing
                     ) vars


readCache :: State -> ([(History, Int)], Int)
readCache s = writeLog ("CACHE readed: " ++ show (length cached) ++ " variants of depth " ++
                            show (if null cached then 0 else length $ fst (head cached))) $
              case cached of
                  [] -> ([([s], 0)], 0)
                  _  -> (cached, length $ fst (head cached))
              where
                cached = mapMaybe (\var -> if length var > 1 && compareTurns (last var) s
                                           then Just (var, 0)
                                           else Nothing
                                  ) $ aiCache s


compareTurns :: State -> State -> Bool
compareTurns s1 s2 = figures s1 == figures s2 && turn s1 == turn s2
--compareTurns s1 s2 = cursor s1 == cursor s2 && getSelectedFigure s1 == getSelectedFigure s2


showVariants :: [([State], Int)] -> String
showVariants vs = intercalate ", " $ map (\gr -> show (snd gr) ++ ": " ++ show (fst gr)) ratedGroups
  where
    eq a b      = snd a == snd b
    cmp a b     = compare (snd a) (snd b)
    groups      = groupBy eq (sortBy cmp vs)
    ratedGroups = map (\gr -> (length gr, snd $ head gr)) groups