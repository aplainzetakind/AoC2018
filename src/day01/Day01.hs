module Day01 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Stream
import Data.Void
import qualified Data.IntSet as S
import Data.Maybe

file = readFile "input"

type Parser = Parsec Void String

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

ops :: String -> Maybe [Int -> Int]
ops = parseMaybe addLines

sumLines :: [Int -> Int] -> Int
sumLines = foldl (flip ($)) 0

sumsLines :: [Int -> Int] -> [Int]
sumsLines = scanl (flip ($)) 0

firstDuplicate :: [Int] -> Maybe Int
firstDuplicate xs = go xs S.empty
                      where go (x:xs) s | x `S.member` s = Just x
                                        | otherwise      = go xs (x `S.insert` s)
                            go [] s                      = Nothing

problem1 :: IO (Maybe Int)
problem1 = fmap sumLines . ops <$> file

problem2 :: IO (Maybe Int)
problem2 = (firstDuplicate =<<) . fmap (sumsLines . cycle) . ops <$> file

main :: IO ()
main = problem1 >>= print >> problem2 >>= print
