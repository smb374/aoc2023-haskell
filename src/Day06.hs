module Day06 (Day06 (..)) where

import Classes (Day (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Char (isDigit)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import GHC.Float (ceilingDouble, int2Double)

data Day06 = Day06

instance Day Day06 where
    inputPath _ = "data/day06/input"
    part1 _ = maybe 0 (L.foldl' (*) 1 . (uncurry cal <$>)) . parseInput . C.lines
    part2 _ = maybe 0 (uncurry cal) . parseInput' . C.lines

cal :: Int -> Int -> Int
cal tMax d =
    if t * (tMax - t) - d == 0
        then b - 1
        else b + 1
  where
    tMax' = int2Double tMax
    d' = int2Double d
    t = ceilingDouble $ (tMax' - sqrt (tMax' ^ (2 :: Int) - 4 * d')) / 2
    b = tMax - t * 2

parseInput :: [ByteString] -> Maybe [(Int, Int)]
parseInput ss = case mapMaybe ((fst <$>) . C.readInt) . C.split ' ' <$> ss of
    [a, b] -> Just $ zip a b
    _else -> Nothing

parseInput' :: [ByteString] -> Maybe (Int, Int)
parseInput' ss = case (fst <$>) . C.readInt . C.filter isDigit <$> ss of
    [Just a, Just b] -> Just (a, b)
    _else -> Nothing
