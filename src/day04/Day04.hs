module Day04 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Stream
import Control.Monad
import Data.Void
import qualified Data.Map.Strict as M
import Data.Maybe
import Data.List
import Data.Ord
import System.Environment

type Parser = Parsec Void String

data Time = T { year   :: Int
              , month  :: Int
              , day    :: Int
              , hour   :: Int
              , minute :: Int } deriving (Eq, Ord, Show)

number :: Parser Int
number = read <$> many digitChar

time :: Parser Time
time = do char '['
          y <- number <* char '-'
          m <- number <* char '-'
          d <- number <* space
          h <- number <* char ':'
          n <- number <* string "] "
          pure (T y m d h n)

sleep :: Parser String
sleep = string "falls asleep"

wake :: Parser String
wake = string "wakes up"

end :: Parser ()
end = void eol <|> eof

shiftBegin = do time >> string "Guard #"
                gid <- number <* space
                string "begins shift" >> end
                pure gid

sleepCycle = try $ do start <- minute <$> time <* sleep <* end
                      end   <- minute <$> time <* wake <* end
                      pure [start..end - 1]

shift :: Parser (Int, [[Int]])
shift = do gid <- shiftBegin
           cs <- many sleepCycle
           pure (gid, cs)

getTime :: String -> Time
getTime = fromMaybe (error "something happened")
                  . parseMaybe (time <* takeRest)

sortLines :: String -> String
sortLines = unlines . sortOn getTime . lines

getShifts :: String -> [(Int, [[Int]])]
getShifts = fromMaybe (error "something happened")
                        . parseMaybe (many shift) . sortLines

accumulateMinutes :: [(Int,[[Int]])] -> [(Int, [[Int]])]
accumulateMinutes = M.toList . fmap (group . sort . concat)
           . foldl (flip $ uncurry $ M.insertWith (++)) (M.fromList [])

findSleepiest :: [(Int, [[Int]])] -> (Int, [[Int]])
findSleepiest = head . sortOn (Down . sum . fmap length . snd)

selectMinute1 :: (Int, [[Int]]) -> (Int, Int)
selectMinute1 = fmap $ head . maximumBy (comparing length)

selectMinute2 :: [(Int, [[Int]])] -> (Int, Int)
selectMinute2 = fmap head
              . maximumBy (comparing $ length . snd)
              . (fmap . fmap) (maximumBy $ comparing length)
              . filter ((/= []) . snd)

main :: IO ()
main = do gs <- head <$> getArgs >>= fmap (accumulateMinutes . getShifts)
                                                                    . readFile
          let star1 = uncurry (*) . selectMinute1 . findSleepiest $ gs
          let star2 = uncurry (*) . selectMinute2 $ gs
          print star1
          print star2
          pure ()
