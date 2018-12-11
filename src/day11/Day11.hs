module Day11 where

import Data.List
import Data.Ord
import Data.Maybe
import qualified Data.Map as M

serial :: Int
serial = 8199

size :: Int
size = 300

cellValue s (i, j) = subtract 5 . (`mod` 10) . (`quot` 100)
                     . (* rackID) . (+ s) . (* j) $ rackID
                        where rackID = i + 10

subRow s (i, j) = sum [ cellValue s (x, j) | x <- [i..size] ]

subMatrix s (i, j) = sum [ subRow s (i, y) | y <- [j..size] ]

memos :: M.Map (Int, Int) Int
memos = M.fromList [ ((i, j), subMatrix serial (i, j)) | i <- [0..size]
                                                       , j <- [0..size] ]

lookup' k m = fromMaybe (error ("lookup failed: " ++ show k)) $ M.lookup k m

nByN (i, j) n = lookup' (i, j) memos         - lookup' (i + n, j) memos
              - lookup' (i, j + n) memos + lookup' (i + n, j + n) memos

largestSquare s = maximumBy (comparing snd)
          [ ((i, j, n), nByN (i, j) n) | i <- [0..size - 1]
                                         , j <- [0..size - 1]
                                         , n <- [1..min (size - i) (size - j)] ]

main :: IO ()
main = print $ largestSquare serial
