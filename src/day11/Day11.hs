module Day11 where

import Data.List
import Data.Ord

serial = 8199
size = 300

cellValue s (i, j) = subtract 5 . (`mod` 10) . (`quot` 100)
                     . (* rackID) . (+ s) . (* j) $ rackID
                        where rackID = i + 10

subRow s (i, j) = sum [ cellValue s (x, j) | x <- [i..size - 1] ]

subMatrix s (i, j) = sum [ subRow s (i, y) | y <- [j..size - 1] ]

nByN s (i, j) n = subMatrix s (i, j)         - subMatrix s (i + n - 1, j)
                - subMatrix s (i, j + n - 1) + subMatrix s (i + n - 1, j + n - 1)

largestSquare s = maximumBy (comparing snd)
          [ ((i, j, n), nByN s (i, j) n) | i <- [0..size - 1]
                                         , j <- [0..size - 1]
                                         , n <- [1..min (size - i) (size - j)] ]

main :: IO ()
main = print $ largestSquare serial
