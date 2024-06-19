module Day02 (Day02 (..)) where

import Classes (Day (..))
import Control.Arrow (first)
import Control.Monad (liftM2, (<=<), (>=>))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe)

data Day02 = Day02

instance Day Day02 where
    inputPath _ = "data/day02/input"
    part1 _ = L.foldl' (+) 0 . mapMaybe (parseLine >=> validGame (12, 13, 14)) . C.lines
    part2 _ =
        L.foldl' (+) 0
            . (foldTup3 (*) . minCubeRequirement . snd <$>)
            . mapMaybe parseLine
            . C.lines

validGame :: (Int, Int, Int) -> (Int, [[ByteString]]) -> Maybe Int
validGame limit (gameId, outcomes) =
    aux . L.foldl' (zipTup3 max) (-1, -1, -1) $ parseOutcome <$> outcomes
  where
    aux current = if foldTup3 (&&) (zipTup3 (<=) current limit) then Just gameId else Nothing

minCubeRequirement :: [[ByteString]] -> (Int, Int, Int)
minCubeRequirement = L.foldl' (zipTup3 max) (-1, -1, -1) . (parseOutcome <$>)

zipTup3 :: (a -> b -> c) -> (a, a, a) -> (b, b, b) -> (c, c, c)
zipTup3 f (a1, a2, a3) (b1, b2, b3) = (f a1 b1, f a2 b2, f a3 b3)

foldTup3 :: (a -> a -> a) -> (a, a, a) -> a
foldTup3 f (x, y, z) = f (f x y) z

-- "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
-- -> Just (1, ["3 blue, 4 red", "1 red, 2 green, 6 blue", "2 green"])
splitGame :: ByteString -> Maybe (Int, [ByteString])
splitGame =
    uncurry ((. pure) . liftM2 (,))
        <=< (first ((fst <$>) . C.readInt . C.drop 5) <$>)
            . L.uncons
            . (C.strip <$>)
            . C.splitWith ((||) <$> (== ':') <*> (== ';'))

-- "Game 1: 3 blue, 4 red; 1 red, 2 green, 6 blue; 2 green"
-- -> Just (1, ["3 blue, 4 red", "1 red, 2 green, 6 blue", "2 green"])
-- -> Just (1, [["3 blue", "4 red"], ["1 red", "2 green", "6 blue"], ["2 green"]])
parseLine :: ByteString -> Maybe (Int, [[ByteString]])
parseLine =
    ((((C.strip <$>) . C.splitWith (== ',') <$>) <$>) <$>) . splitGame

-- ["1 red", "2 green", "6 blue"] -> (1, 2, 6)
parseOutcome :: [ByteString] -> (Int, Int, Int)
parseOutcome =
    L.foldl'
        (\acc -> zipTup3 (+) acc . fromMaybe (0, 0, 0) . parseItem)
        (0, 0, 0)
  where
    parseItem =
        C.readInt
            >=> \(f, color) -> case C.unpack $ C.strip color of
                "red" -> Just (f, 0, 0)
                "green" -> Just (0, f, 0)
                "blue" -> Just (0, 0, f)
                _ -> Nothing
