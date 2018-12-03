module Day03 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Stream
import Data.Void
import qualified Data.Set as S
import Data.Maybe

type Parser = Parsec Void String

data Corners = C Int Int Int Int deriving Show

file = readFile "input"

number :: Parser Int
number = read <$> many digitChar

first :: Parser Int
first = string "#" *> many digitChar
                   *> string " @ "
                   *> number <* string ","

second :: Parser Int
second = fmap read $ many digitChar <* string ": "

third :: Parser Int
third = number <* string "x"

fourth :: Parser Int
fourth = number

corners = C <$> first <*> second <*> third <*> fourth

getCorners :: String -> Corners
getCorners = fromMaybe (error "something happened") . parseMaybe corners

pairsInside :: Corners -> S.Set (Int, Int)
pairsInside (C x y w h) = S.fromList
                        [(i, j) | i <- [x + 1..x + w], j <- [y + 1..y + h]]

process :: [Corners] -> S.Set (Int, Int)
process = go S.empty S.empty
       where go ones dones []     = dones
             go ones dones (c:cs) = let c' = pairsInside c `S.difference` dones
                                        s  = S.intersection ones c'
                                        s2 = S.union dones s
                                        s1 = S.difference (S.union ones c') s
                                    in  go s1 s2 cs

main :: IO ()
main = do pairs <- fmap (\l -> (tail . head . words $ l, getCorners l))
                              . lines <$> file
          let multpoints = process . fmap snd $ pairs
          print $ length multpoints
          putStrLn . fst . head
              $ filter ((== S.empty) .S.intersection multpoints
              . pairsInside . snd) pairs
