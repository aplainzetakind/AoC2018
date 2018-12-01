module Day01 where

import Text.Megaparsec
import Text.Megaparsec.String
import qualified Data.Set as S
import Data.Maybe

-- input file
file = readFile "input"

-- parsers
plus :: Num a => Parser (a -> a -> a)
plus = const (+) <$> char '+'

minus :: Num a => Parser (a -> a -> a)
minus = const (flip (-)) <$> char '-'

digits :: Parser Int
digits = read <$> many digitChar

addLine :: Parser (Int -> Int)
addLine = (plus <|> minus) <*> digits <* eol

addLines :: Parser [Int -> Int]
addLines = many addLine

-- result of file parse
ops :: String -> Maybe [Int -> Int]
ops = parseMaybe addLines

sumLines :: [Int -> Int] -> Int
sumLines = foldl (flip ($)) 0

sumsLines :: [Int -> Int] -> [Int]
sumsLines = scanl (flip ($)) 0

firstDuplicate :: (Eq a, Ord a) => [a] -> Maybe a
firstDuplicate xs = go xs S.empty
                      where go [] s                      = Nothing
                            go (x:xs) s | x `S.member` s = Just x
                                        | otherwise      = go xs (x `S.insert` s)

problem1 :: IO (Maybe Int)
problem1 = fmap sumLines . ops <$> file

problem2 :: IO (Maybe Int)
problem2 = (firstDuplicate =<<) . fmap (sumsLines . cycle) . ops <$> file
