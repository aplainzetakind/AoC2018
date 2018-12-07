module Day06 where

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Stream
import Data.Void
import Data.Maybe
import Data.List
import qualified Data.Set as S
import Data.Ord
import System.Environment

type Parser = Parsec Void String

data Point = P { px :: Int, py :: Int } deriving (Eq, Ord)

instance Show Point where
  show (P x y) = "(" ++ show x ++ ", " ++ show y ++ ")"

l1 :: Point -> Point -> Int
l1 (P x1 y1) (P x2 y2) = abs (x2 - x1) + abs (y2 - y1)

number :: Parser Int
number = read <$> many digitChar

point :: Parser Point
point = do x <- number
           string ", "
           P x <$> number

getList :: String -> [Point]
getList = fromMaybe (error "something happened") . traverse id
        . fmap (parseMaybe point) . lines

getLimits :: Ord b => (a -> a -> Bool, a -> b) -> [a] -> [a]
getLimits (f, s) = go [] . sortOn s
                 where go ps' [] = ps'
                       go ps' (x:xs) = go (x:ps') (filter (f x) xs)

left :: (Point -> Point -> Bool, Point -> Int)
left = (\x p -> py p - px p > py x - px x || px p + py p < px x + py x,
        px)

top :: (Point -> Point -> Bool, Point -> Int)
top = (\x p -> py p - px p > py x - px x || px p + py p > px x + py x,
       negate . py)

right :: (Point -> Point -> Bool, Point -> Int)
right = (\x p -> py p - px p < py x - px x || px p + py p > px x + py x,
         negate . px)

bottom :: (Point -> Point -> Bool, Point -> Int)
bottom = (\x p -> py p - px p < py x - px x || px p + py p < px x + py x,
          py)

box :: [Point] -> [Point]
box ps = P <$> [xm..xM] <*> [ym..yM]
                where xm = minimum $ px <$> ps
                      xM = maximum $ px <$> ps
                      ym = minimum $ py <$> ps
                      yM = maximum $ py <$> ps

data FoldStatus a = NotStarted | Folding a

minimumBy' :: Foldable t => (a -> a -> Ordering) -> t a -> Maybe a
minimumBy' f = result . foldl folder NotStarted
          where folder NotStarted x = Folding (Just x, x)
                folder v@(Folding (_, a)) x | f x a == LT = Folding (Just x, x)
                                            | f x a == EQ = Folding (Nothing, x)
                                            | otherwise   = v
                result NotStarted  = error "minimumBy': empty structure"
                result (Folding v) = fst v

closestOf :: Foldable t => t Point -> Point -> Maybe Point
closestOf ps p = minimumBy' (comparing (l1 p)) ps

totalDistance :: (Functor t, Foldable t) => t Point -> Point -> Int
totalDistance ps p = sum $ fmap (l1 p) ps

getLandlocked :: [Point] -> S.Set Point
getLandlocked ps = let ext = S.fromList . concat
                            . traverse getLimits [left, top, right, bottom] $ ps
                       all = S.fromList ps
                   in all S.\\ ext

main :: IO ()
main = do points <- head <$> getArgs >>= fmap getList . readFile
          let candidates = getLandlocked points
              toCheck    = box points
              star1      = maximum . fmap length . group . sort
                           . filter (`S.member` candidates)
                           . catMaybes $ closestOf points <$> toCheck
              star2      = length . filter (< 10000)
                           $ totalDistance points <$> toCheck
          print star1 >> print star2
          pure ()
