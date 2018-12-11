module Day10 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Char.Lexer hiding (space)
import Text.Megaparsec.Stream
import Control.Monad.State
import Data.Void
import Data.List
import Data.Maybe
import Data.Function
import System.Environment


data Light = L { px :: Int
               , py :: Int
               , vx :: Int
               , vy :: Int } deriving (Eq, Ord, Show)

type Parser = Parsec Void String

light :: Parser Light
light = do string "position=<" >> space
           px <- signed space decimal
           string "," >> space
           py <- signed space decimal
           string "> velocity=<" >> space
           vx <- signed space decimal
           string "," >> space
           vy <- signed space decimal
           string ">" >> eol
           pure (L px py vx vy)

getLight :: String -> [Light]
getLight = fromMaybe (error "parse failed") . parseMaybe (many light)

display :: [Light] -> String
display ls = concat [ [full i j | i <- [xm..xM] ] ++ "\n" | j <- [ym..yM] ]
            where points = (\(L a b _ _) -> (a, b)) <$> ls
                  xm = minimum (fst <$> points)
                  xM = maximum (fst <$> points)
                  ym = minimum (snd <$> points)
                  yM = maximum (snd <$> points)
                  full i j = if (i, j) `elem` points then '#' else '.'

findFirstMinimumOn :: Ord b => (a -> b) -> [a] -> (a, Int)
findFirstMinimumOn _ []       = error "findFirstMinimumOn: empty list"
findFirstMinimumOn f (x : xs) = go x 0 xs
                          where go y n [] = (y, n)
                                go y n (z : zs) = if f z > f y
                                                  then (y, n)
                                                  else go z (n+1) zs

move :: Light -> Light
move (L x y a b) = L (x + a) (y + b) a b

dimensions :: [Light] -> (Int, Int)
dimensions ls = (x, y) where x = maximum (px <$> ls) - minimum (px <$> ls)
                             y = maximum (py <$> ls) - minimum (py <$> ls)

height :: [Light] -> Int
height ls = x where x = maximum (px <$> ls) - minimum (px <$> ls)

main = do lights <- head <$> getArgs >>= fmap getLight . readFile
          let (msg, index) = findFirstMinimumOn height
                             $ iterate (fmap move) lights
          putStrLn $ display msg
          print index
