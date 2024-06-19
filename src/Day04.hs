{-# LANGUAGE LambdaCase #-}

module Day04 (Day04 (..)) where

import Classes (Day (..))
import Control.Monad ((>=>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import Data.List (uncons)
import qualified Data.List as L
import Data.Maybe (mapMaybe)
import qualified Data.Vector.Unboxed as V

data Day04 = Day04

instance Day Day04 where
    inputPath _ = "data/day04/input"
    part1 _ = L.foldl' (+) 0 . (calculatePoints <$>) . mapMaybe buildCard . C.lines
    part2 _ = calculateCopies . mapMaybe buildCard . C.lines

type Card = (IntSet, IntSet)

if' :: Bool -> a -> a -> a
if' p x y = if p then x else y

-- "Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53"
-- -> Just ["41 48 83 86 17", "83 86  6 31 17  9 48 53"]
splitCardId :: ByteString -> Maybe [ByteString]
splitCardId =
    (snd <$>)
        . uncons
        . (C.strip <$>)
        . C.splitWith ((||) <$> (== ':') <*> (== '|'))

buildCard :: ByteString -> Maybe Card
buildCard =
    splitCardId >=> \case
        [wNums, mNums] -> Just (toIntSet wNums, toIntSet mNums)
        _rest -> Nothing
  where
    toIntSet = IS.fromList . mapMaybe ((fst <$>) . C.readInt . C.strip) . C.split ' '

calculatePoints :: Card -> Int
calculatePoints (ws, ms) =
    case items of
        0 -> 0
        n -> 2 ^ (n - 1)
  where
    items = IS.size $ IS.intersection ws ms

calculateCopies :: [Card] -> Int
calculateCopies cards =
    V.sum $
        V.foldl'
            ( \acc (x, y) ->
                V.imap (\i v -> v + if' (i > x && i <= x + y) (acc V.! x) 0) acc
            )
            (V.replicate (V.length matches) 1)
            matches
  where
    matches = V.fromList $ zip [0 ..] $ IS.size . uncurry IS.intersection <$> cards
