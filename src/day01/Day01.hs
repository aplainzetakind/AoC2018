module Day01 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Stream
import System.Environment
import Data.Void
import qualified Data.IntSet as S
import Data.Maybe

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

star1 :: String -> Int
star1 = maybe (error "something happened") sumLines . ops

star2 :: String -> Int
star2 = fromMaybe (error "something happened") . (firstDuplicate =<<)
           . fmap (sumsLines . cycle) . ops

main :: IO ()
main = do contents <- readFile . head =<< getArgs
          print $ star1 contents
          print $ star2 contents
