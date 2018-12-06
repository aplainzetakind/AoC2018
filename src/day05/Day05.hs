module Day05 where

import System.Environment
import Data.Char
import Data.Bits

consume :: String -> String
consume = foldr reduce' []
                  where
                    reduce' x []     = [x]
                    reduce' x (y:ys) = if ord x `xor` ord y == 32
                                       then ys
                                       else x : y : ys

resultLength :: String -> Int
resultLength = length . consume

filteredFrom :: String -> Char -> String
filteredFrom s a = filter ((/= a) . toUpper) s

main :: IO ()
main = do input <- head <$> getArgs >>= fmap (concat . lines) . readFile
          let star1 = resultLength input
          let star2 = minimum
                        $ fmap (resultLength . filteredFrom input) ['A'..'Z']
          print star1
          print star2
          pure ()
