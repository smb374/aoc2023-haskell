module Day01 (Day01 (..)) where

import Control.Applicative ((<|>))
import Control.Monad (mfilter)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Char (isDigit)
import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe)

import Classes (Day (..))

data Day01 = Day01

instance Day Day01 where
    inputPath _ = "data/day01/input"
    part1 _ =
        L.foldl' (+) 0
            . mapMaybe ((fst <$>) . C.readInt . trimDigits . C.filter isDigit)
            . C.lines
    part2 _ =
        L.foldl' (+) 0
            . mapMaybe ((fst <$>) . C.readInt . trimDigits . process)
            . C.lines

trimDigits :: ByteString -> ByteString
trimDigits s = case C.length s of
    0 -> C.singleton '0'
    1 -> s <> s
    2 -> s
    _ -> C.concat $ C.singleton <$> [C.head s, C.last s]

trySubString :: ByteString -> Int -> Int -> Maybe ByteString
trySubString str idx len
    | idx + len > C.length str = Nothing
    | otherwise = Just $ C.take len (C.drop idx str)

wordDigit :: ByteString -> Maybe ByteString
wordDigit s = C.singleton <$> res
  where
    res = case C.unpack s of
        "eight" -> Just '8'
        "five" -> Just '5'
        "four" -> Just '4'
        "nine" -> Just '9'
        "one" -> Just '1'
        "seven" -> Just '7'
        "six" -> Just '6'
        "three" -> Just '3'
        "two" -> Just '2'
        _ -> Nothing

getDigits :: ByteString -> Int -> Maybe ByteString
getDigits str idx = singleDigit <|> firstMatch possibleMatches
  where
    firstMatch [] = Nothing
    firstMatch (x : xs) = wordDigit x <|> firstMatch xs
    singleDigit = C.singleton <$> mfilter isDigit (C.indexMaybe str idx)
    possibleMatches = mapMaybe (trySubString str idx) [3, 4, 5]

process :: ByteString -> ByteString
process s =
    L.foldl'
        (\acc idx -> acc <> fromMaybe C.empty (getDigits s idx))
        C.empty
        [0 .. C.length s - 1]
