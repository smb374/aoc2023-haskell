{-# LANGUAGE OverloadedStrings #-}

module Day08 (Day08 (..)) where

import Classes (Day (..))
import Control.Monad ((<=<))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty)
import qualified Data.List.NonEmpty as LN
import Data.Map.Strict (Map, (!?))
import qualified Data.Map.Strict as M

data Day08 = Day08

instance Day Day08 where
    inputPath _ = "data/day08/input"
    part1 _ = solve walk
    part2 _ = solve walk'

type CamelMap = Map ByteString (ByteString, ByteString)

data Direction = L | R deriving (Show, Eq)

solve :: (CamelMap -> (NonEmpty Direction, Int) -> Int) -> ByteString -> Int
solve walkF bs = case filter (/= "") $ C.lines bs of
    (dir : ls) -> maybe 0 (walkF (L.foldl' (flip parseLine) M.empty ls)) (parseDirection dir)
    _ -> 0

walk :: CamelMap -> (NonEmpty Direction, Int) -> Int
walk cm ds = maybe 0 snd $ generalWalk cm ds (== "ZZZ") "AAA"

walk' :: CamelMap -> (NonEmpty Direction, Int) -> Int
walk' cm ds = maybe 0 (L.foldl' lcm 1) walkSteps
  where
    walkSteps = mapM ((snd <$>) . generalWalk cm ds ((== 'Z') . C.last)) =<< getStartNodes cm

generalWalk :: CamelMap -> (NonEmpty Direction, Int) -> (ByteString -> Bool) -> ByteString -> Maybe (ByteString, Int)
generalWalk cm (ds, dsl) stop start = f start 0
  where
    f node n = step cm ds node >>= \x -> if stop x then return (x, n + dsl) else f x (n + dsl)

step :: CamelMap -> NonEmpty Direction -> ByteString -> Maybe ByteString
step cm ds node = L.foldl' (\acc d -> child cm d =<< acc) (Just node) ds

child :: CamelMap -> Direction -> ByteString -> Maybe ByteString
child m L = (fst <$>) . (m !?)
child m R = (snd <$>) . (m !?)

char2dir :: Char -> Maybe Direction
char2dir 'L' = Just L
char2dir 'R' = Just R
char2dir _ = Nothing

-- "AAA = (BBB, CCC)" -> M.insert "AAA" ("BBB", "CCC") m
-- take 3, drop 4, take 3, drop 2, take 3
parseLine :: ByteString -> CamelMap -> CamelMap
parseLine bs = M.insert node (lchild, rchild)
  where
    (node, rest) = C.splitAt 3 bs
    (lchild, rest') = C.splitAt 3 $ C.drop 4 rest
    rchild = C.take 3 $ C.drop 2 rest'

parseDirection :: ByteString -> Maybe (NonEmpty Direction, Int)
parseDirection = ((<*>) . ((,) <$>) . (LN.nonEmpty <=< mapM char2dir . C.unpack)) <*> (pure . C.length)

-- parseDirection = (liftM2 (,) . (LN.nonEmpty <=< mapM char2dir . C.unpack)) <*> (pure . C.length)

getStartNodes :: CamelMap -> Maybe (NonEmpty ByteString)
getStartNodes = LN.nonEmpty . (fst <$>) . M.toList . M.filterWithKey (curry ((== 'A') . C.last . fst))
