module Day09 (Day09 (..)) where

import Classes (Day (..))

import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.List as L
import Data.Sequence (Seq (..))
import qualified Data.Sequence as S

data Day09 = Day09

instance Day Day09 where
    inputPath _ = "data/day09/input"
    part1 _ = solve getNext
    part2 _ = solve getNext'

solve :: (Seq Int -> Maybe Int) -> ByteString -> Int
solve nextF = maybe 0 (L.foldl' (+) 0) . mapM (nextF . parseLine) . C.lines

getNext :: Seq Int -> Maybe Int
getNext Empty = Nothing
getNext (xsh :<| Empty) = Just xsh
getNext xs@(xsh :<| xst@(_ :|> xsl))
    | L.all (== 0) xs' = Just xsh
    | otherwise = (+ xsl) <$> getNext xs'
  where
    xs' = S.zipWith (-) xst xs

getNext' :: Seq Int -> Maybe Int
getNext' Empty = Nothing
getNext' xs@(xsh :<| xst)
    | L.all (== 0) xs' = Just xsh
    | otherwise = (xsh -) <$> getNext' xs'
  where
    xs' = S.zipWith (-) xst xs

-- "0 3 6 9 12 15" -> Just ([0,3,6,9,12,15])
parseLine :: ByteString -> Seq Int
parseLine = S.unfoldr (C.readInt . C.dropSpace)
