module Day03 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Stream
import Data.Void
import qualified Data.Set as S
import Data.Maybe
import System.Environment

type Parser = Parsec Void String

data Corners = C { idt    :: Int
                 , top    :: Int
                 , bottom :: Int
                 , width  :: Int
                 , height :: Int } deriving Show

file = readFile "input"

number :: Parser Int
number = read <$> many digitChar

corner = do char '#'
            i <- number <* string " @ "
            x <- number <* char ','
            y <- number <* string ": "
            w <- number <* char 'x'
            h <- number <* (const () <$> eol <|> eof)
            pure (C i x y w h)

getCorners :: String -> [Corners]
getCorners = fromMaybe (error "something happened") . parseMaybe (many corner)

pairsInside :: Corners -> S.Set (Int, Int)
pairsInside (C _ x y w h) = S.fromList
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
main = do corners <- getCorners <$> (readFile =<< head <$> getArgs)
          let overlaps = process corners
          print $ S.size overlaps
          print . idt . head
              $ filter ((== S.empty) . S.intersection overlaps . pairsInside)
                       corners
