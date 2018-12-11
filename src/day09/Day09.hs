module Day09 where

import qualified Data.IntMap as M
import Data.List
import Data.Ord
import qualified Data.Sequence as S
import System.Environment

rotate :: Eq a => Int -> S.Seq a -> S.Seq a
rotate n xs | xs == S.empty     = S.empty
            | n >= 0            = S.drop n xs S.>< S.take n xs
            | n <  0            = rotate m xs
                        where m = length xs + n

initScore :: Int -> M.IntMap Int
initScore n = M.fromList $ (,) <$> [0..n - 1] <*> [0]

winner :: M.IntMap Int -> Int
winner = maximum . fmap snd . M.toList

play :: Int -> Int -> Int
play players limit = go (initScore players) 1 (S.singleton 1)
              where go scoreBoard stone circle
                         | stone == limit + 1  = winner scoreBoard
                         | stone `mod` 23 == 0 = go s' (stone + 1) c'
                         | otherwise = go scoreBoard (stone + 1) c''
                               where (x S.:< xs) = S.viewl $ rotate (-7) circle
                                     c' = xs
                                     s' = M.insertWith (+) (stone `mod` players)
                                                         (stone + x) scoreBoard
                                     c'' = stone S.<| rotate 2 circle

main :: IO ()
main = do (players : marbles : _) <- fmap read <$> getArgs
          print $ play players marbles
