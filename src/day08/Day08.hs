module Day08 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Stream
import Data.Void
import Data.Maybe
import Control.Monad
import System.Environment

data Tree = Node { metadata :: [Int]
                 , children :: [Tree] } deriving (Eq, Show)

sumMetadata :: Tree -> Int
sumMetadata (Node ms cs) = sum ms + sum (fmap sumMetadata cs)

value :: Tree -> Int
value (Node ms []) = sum ms
value (Node ms cs) = sum . fmap value . catMaybes $ fmap (safeIndex cs) ms

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs n | n <= 0 = Nothing
               | n > length (take n xs) = Nothing
               | otherwise = Just (xs !! (n - 1))

type Parser = Parsec Void String

number :: Parser Int
number = read <$> many digitChar <* space

tree :: Parser Tree
tree = do cn <- number
          mn <- number
          ts <- replicateM cn tree
          ms <- replicateM mn number
          pure (Node ms ts)

getTree = fromMaybe (error "getTree failed") . parseMaybe tree

main :: IO ()
main = do tr <- fmap getTree $ getArgs >>= readFile . head
          let star1 = sumMetadata tr
              star2 = value tr
          print star1 >> print star2
