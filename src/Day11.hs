{-# LANGUAGE TupleSections #-}

module Day11 (Day11 (..)) where

import Classes (Day (..))

import Control.Arrow (Arrow (first, second), (***))
import Control.Monad (join)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.IntSet (IntSet)
import qualified Data.IntSet as IS
import qualified Data.List as L
import Data.Tuple (swap)

data Day11 = Day11

instance Day Day11 where
    inputPath _ = "data/day11/input"

    part1 _ = flip walk 0 . uncurry (calculateNewCoords 1) . parseInput
    part2 _ = flip walk 0 . uncurry (calculateNewCoords 999999) . parseInput

type Dimension = (Int, Int)
type Point = (Int, Int)

if' :: Bool -> a -> a -> a
if' cond x y = if cond then x else y

parseLine :: Int -> ByteString -> [Point] -> [Point]
parseLine row = flip $ f 0
  where
    f col acc = maybe acc (\(c, s') -> f (col + 1) (if' (c == '#') ((row, col) : acc) acc) s') . C.uncons

parseInput :: ByteString -> ([Point], Dimension)
parseInput = (,) <$> L.foldl' (flip (uncurry parseLine)) [] . zip [0 ..] . C.lines <*> getDimension

getDimension :: ByteString -> Dimension
getDimension = swap . maybe (0, 0) (C.length *** ((1 +) . L.length)) . L.uncons . C.lines

occupiedCoords :: [Point] -> (IntSet, IntSet)
occupiedCoords = (IS.fromList *** IS.fromList) . unzip

expandCoords :: [Point] -> Dimension -> (IntSet, IntSet)
expandCoords ps (rlim, clim) =
    ( IS.fromList $ filter (not . flip IS.member orow) [0 .. rlim - 1]
    , IS.fromList $ filter (not . flip IS.member ocol) [0 .. clim - 1]
    )
  where
    (orow, ocol) = occupiedCoords ps

calculateNewCoords :: Int -> [Point] -> Dimension -> [Point]
calculateNewCoords offset ps dim = (snd <*> fst) <$> transformed ps
  where
    transformed = uncurry (.) . (foldF stackRow *** foldF stackCol) . flip expandCoords dim <*> ((,id) <$>)

    foldF f = flip (IS.foldl' ((. f) . flip fmap))

    stackRow r (p, f) = (p, if' (fst p > r) (first (+ offset) . f) f)
    stackCol c (p, f) = (p, if' (snd p > c) (second (+ offset) . f) f)

dist :: Point -> Point -> Int
dist = (uncurry (+) .) . uncurry (***) . join (***) ((abs .) . subtract)

walk :: [Point] -> Int -> Int
walk [] n = n
walk (p : ps) n = (walk <*> L.foldl' ((. dist p) . (+)) n) ps
