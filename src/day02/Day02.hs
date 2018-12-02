module Day02 where

import Data.List

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

differ :: (Eq a, Num t) => [a] -> [a] -> (t, [a])
differ = go 0 []
           where go n rs xs ys = case (xs, ys) of
                                  (a:as, b:bs) | a == b -> go n (a:rs) as bs
                                               | otherwise -> go (n + 1) rs as bs
                                  ([],[]) -> (n, reverse rs)
                                  _ -> error "something happened"

findOneDiff :: (Applicative t, Foldable t) => t String -> String
findOneDiff l = maybe "something happened" snd . find ((== 1) . fst)
                                            $ differ <$> l <*> l

star2 :: IO String
star2 = findOneDiff . lines <$> file

main :: IO ()
main = star1 >>= print >> star2 >>= print
