module Day02 where

import Data.List
import qualified Data.Set as S
import Control.Monad
import Data.Maybe

file :: IO String
file = readFile "input"

occursTimes :: Ord a => Int -> [a] -> Bool
occursTimes n = elem n . map length . group . sort

ifOccursTimesAdd n l | occursTimes n l = (+1)
                     | otherwise         = id

countBoth :: (Traversable t, Ord a) => t [a] -> (Int, Int)
countBoth = foldl f (0,0)
             where f (a, b) str = ( ifOccursTimesAdd 2 str a
                                  , ifOccursTimesAdd 3 str b)

star1 :: IO Int
star1 = uncurry (*) . countBoth . lines <$> file

skip :: Int -> String -> String
skip n cs = take (n - 1) cs ++ drop n cs

firstDuplicate :: [String] -> Maybe String
firstDuplicate xs = go xs S.empty
                     where go (x:xs) s | x `S.member` s = Just x
                                       | otherwise      = go xs (x `S.insert` s)
                           go [] s                      = Nothing

findMatch :: [String] -> Maybe String
findMatch ls = msum $ fmap firstDuplicate [fmap (skip n) ls | n <- [1..]]

star2 :: IO String
star2 = fromMaybe "something happened" . findMatch . lines <$> file

main :: IO ()
main = star1 >>= print >> star2 >>= print
