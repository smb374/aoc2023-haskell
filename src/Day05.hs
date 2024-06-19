{-# LANGUAGE RecordWildCards #-}

module Day05 (Day05 (..)) where

import Classes (Day (..))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Semigroup (Min (Min, getMin))
import Data.Vector (Vector)
import qualified Data.Vector as V

data Day05 = Day05

instance Day Day05 where
    inputPath _ = "data/day05/input"
    part1 _ = fromMaybe 0 . minLocation . parseInput . C.lines
    part2 _ = fromMaybe 0 . minLocation' . parseInput' . C.lines

data DataRange = DataRange
    { src :: !Int
    , dst :: !Int
    , rng :: !Int
    }
    deriving (Eq, Show)

-- x-y: seed range, a-b: data range
data Overlap
    = NoOverlap -- x-y-a-b || a-b-x-y
    | Full !Int -- a-x-y-b
    | Start !Int -- a-x-b-y
    | End !Int -- x-a-y-b
    | Over -- x-a-b-y
    deriving (Eq, Show)

data Part = One | Two deriving (Eq, Show)

data InputMap = InputMap
    { seeds :: ![Int]
    , dataMap :: !(Vector [DataRange])
    }
    deriving (Eq, Show)

data InputMap' = InputMap'
    { seeds' :: ![(Int, Int)]
    , dataMap' :: !(Vector [DataRange])
    }
    deriving (Eq, Show)

minimumMaybe :: (Ord a) => [a] -> Maybe a
minimumMaybe xs = case Min . Just <$> xs of
    (x : xs') -> getMin $ L.foldl' (<>) x xs'
    [] -> Nothing

-- "seeds: 79 14 55 13" -> [79, 14, 55, 13]
parseSeeds :: ByteString -> [Int]
parseSeeds = mapMaybe ((fst <$>) . C.readInt) . C.split ' ' . C.drop 7

-- "50 98 2" -> DataMap 98 50 2
parseRange :: ByteString -> Maybe DataRange
parseRange s = case nums of
    [dst, src, rng] ->
        Just (DataRange src dst rng)
    _else -> Nothing
  where
    nums = mapMaybe ((fst <$>) . C.readInt) $ C.split ' ' s

parseSingleMap :: [ByteString] -> [DataRange] -> [DataRange]
parseSingleMap [] rs = rs
parseSingleMap (s : ss) rs = case parseRange s of
    Just d -> parseSingleMap ss (d : rs)
    Nothing -> parseSingleMap ss rs

parseInput :: [ByteString] -> InputMap
parseInput = go (InputMap [] V.empty)
  where
    go im [] = im
    go InputMap{..} ss =
        let (current, rest) = L.uncons <$> L.span ((/= "") . C.unpack) ss
            next = maybe [] snd rest
         in case current of
                [s]
                    | C.unpack (C.take 7 s) == "seeds: " ->
                        go (InputMap (parseSeeds s) dataMap) next
                _else ->
                    let dat = parseSingleMap current []
                     in go (InputMap seeds (V.snoc dataMap dat)) next

isMapped :: Int -> DataRange -> Bool
isMapped x (DataRange src _ rng) = x >= src && x < src + rng

getDst :: Int -> [DataRange] -> Int
getDst x ds = case dropWhile (not . isMapped x) ds of
    (DataRange src dst _ : _) -> dst + (x - src)
    [] -> x

getLocation :: Vector [DataRange] -> Int -> Int
getLocation = flip (V.foldl' getDst)

minLocation :: InputMap -> Maybe Int
minLocation InputMap{..} = minimumMaybe $ getLocation dataMap <$> seeds

parseSeeds' :: ByteString -> [(Int, Int)]
parseSeeds' s = go (parseSeeds s) []
  where
    go xs acc = case xs of
        (start : len : xs') -> go xs' ((start, start + len - 1) : acc)
        _else -> acc

parseInput' :: [ByteString] -> InputMap'
parseInput' = go (InputMap' [] V.empty)
  where
    go im [] = im
    go InputMap'{..} ss =
        let (current, rest) = maybe [] snd . L.uncons <$> L.span ((/= "") . C.unpack) ss
         in case current of
                [s]
                    | C.unpack (C.take 7 s) == "seeds: " ->
                        go (InputMap' (parseSeeds' s) dataMap') rest
                _else ->
                    let dat = parseSingleMap current []
                     in go (InputMap' seeds' (V.snoc dataMap' dat)) rest

getOverlap :: (Int, Int) -> DataRange -> Overlap
getOverlap (x, y) DataRange{..}
    | src <= x && y < src + rng = Full (x - src) -- src-x-y-(src+rng-1)
    | x < src && src <= y && y < src + rng = Start (y - src) -- x-src-y-(src+rng-1)
    | src <= x && x < src + rng && src + rng <= y = End (x - src) -- src-x-(src+rng-1)-y
    | x < src && src + rng <= y = Over -- x-src-(src+rng-1)-y
    | otherwise = NoOverlap

layerStep :: (Int, Int) -> ([(Int, Int)], [(Int, Int)]) -> DataRange -> Maybe ([(Int, Int)], [(Int, Int)])
layerStep s@(x, y) (remain, result) dr@DataRange{..} = case getOverlap s dr of
    Full offset -> Just (remain, (dst + offset, dst + offset + y - x) : result) -- src-x-y-(src+rng-1)
    Start offset -> Just ((x, src - 1) : remain, (dst, dst + offset) : result) -- x-src-y-(src+rng-1)
    End offset -> Just ((src + rng, y) : remain, (dst + offset, dst + rng - 1) : result) -- src-x-(src+rng-1)-y
    Over -> Just ((x, src - 1) : (src + rng, y) : remain, (dst, dst + rng - 1) : result) -- x-src-(src+rng-1)-y
    NoOverlap -> Nothing

layerLoop :: ([(Int, Int)], [(Int, Int)]) -> [DataRange] -> ([(Int, Int)], [(Int, Int)])
layerLoop ([], result) _ = (result, [])
layerLoop (r : remain, result) layer = go r (remain, result) layer
  where
    -- case 1: seed has an overlap with d, continue layerLoop with state'.
    -- case 2: seed has no overlap with d, try next d in ds if possible.
    -- final: seed has no overlap with layer (not mapped), add to result and continue layerLoop.
    go seed state [] = layerLoop ((seed :) <$> state) layer -- final
    go seed state (d : ds) = case layerStep seed state d of
        Just state' -> layerLoop state' layer -- case 1
        Nothing -> go seed state ds -- case 2

seedLocation :: (Int, Int) -> Vector [DataRange] -> [(Int, Int)]
seedLocation seed layers = fst $ V.foldl' layerLoop ([seed], []) layers

minLocation' :: InputMap' -> Maybe Int
minLocation' InputMap'{..} = minimumMaybe $ fst <$> concatMap (`seedLocation` dataMap') seeds'
