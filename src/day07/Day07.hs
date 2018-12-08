module Day07 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Stream
import Data.Void
import Data.List
import Data.Maybe
import Data.Char
import System.Environment

type Parser = Parsec Void String

data Worker = Idle | Task Char Int deriving (Eq, Show)

wTimeLeft Idle = 0
wTimeLeft (Task _ i) = i

wTask (Task c _) = c

data WorkStatus = WS { workers :: [Worker]
                     , done    :: String
                     , ready   :: String
                     , tasks   :: String
                     , deps    :: [(Char, Char)] } deriving Show

numWorkers = 5

elapseSecond :: WorkStatus -> WorkStatus
elapseSecond (WS ws d r t dp) = WS ws' d' r' t' dp'
    where newDone = fmap wTask . filter ((== 1) . wTimeLeft) $ ws
          d'      = foldr (:) d newDone
          dp'     = filter (not . (`elem` newDone) . fst) dp
          r''     = foldr insert r $ filter (not . (`elem` fmap snd dp')) t
          idles   = length $ filter ((<= 1) . wTimeLeft) ws
          newWs   = min idles (length r'')
          ws'     = fmap doSecond (filter ((> 1) . wTimeLeft) ws)
                    ++ (startTask <$> take newWs r'')
                    ++ replicate (idles - newWs) Idle
          r'      = drop newWs r''
          t'      = filter (`elem` fmap snd dp') t

startTask :: Char -> Worker
startTask c = Task c (ord c - 4)

initWork deps = WS (replicate numWorkers Idle) "" "" (taskList deps) deps

work :: [(Char, Char)] -> [WorkStatus]
work = let idles = replicate numWorkers Idle
       in takeWhile ((/= idles) . workers)
        . dropWhile ((== idles) . workers)
        . iterate elapseSecond . elapseSecond . initWork

doSecond :: Worker -> Worker
doSecond Idle = Idle
doSecond (Task c n) = Task c (n - 1)

pair :: Parser (Char, Char)
pair = do string "Step "
          first <- anySingle
          string " must be finished before step "
          second <- anySingle
          string " can begin." >> eol
          pure (first, second)

taskList :: Ord a => [(a, a)] -> [a]
taskList deps = sort . nub $ deps >>= \(a, b) -> [a, b]

build :: [(Char, Char)] -> String
build deps = go [] (taskList deps) deps
                where go r [] _  = reverse r
                      go r ts ps =
                          let a   = fromMaybe (error "something happened")
                                    $ nextTask ts ps
                              ts' = filter (/= a) ts
                              ps' = filter ((/= a) . fst) ps
                          in  go (a : r) ts' ps'

nextTask tasks deps = find (not . (`elem` (snd <$> deps))) tasks

getPairs :: String -> [(Char, Char)]
getPairs = fromMaybe (error "something happened") . parseMaybe (many pair)

main :: IO ()
main = do list <- getPairs <$> (getArgs >>= readFile . head)
          let star1 = build list
              star2 = length . work $ list
          print star1 >> print star2
