module Day03 (Day03 (..)) where

import Classes (Day (..))
import Control.Arrow ((***))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Char (isDigit)
import Data.Containers.ListUtils (nubIntOn)
import Data.IntMap.Strict (IntMap, (!?))
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import Data.Maybe (mapMaybe)

type Symbols = [(Char, (Int, Int))]
type NumberMap = IntMap NumberMapRow
type NumberMapRow = IntMap (Int, Int)
type State = (Int, (NumberMap, Symbols))

data Day03 = Day03

instance Day Day03 where
    inputPath _ = "data/day03/input"
    part1 _ = uncurry walkSymbols . buildMap . C.lines
    part2 _ = uncurry walkSymbols' . buildMap . C.lines

buildMapRow :: State -> Int -> ByteString -> State
buildMapRow (idxNum, (numMap, symbols)) row line = aux 0 idxNum line (IM.empty, symbols)
  where
    aux col n s (nr, ss) = case C.readInt s of
        Just (val, rest) ->
            let col' = col + C.length s - C.length rest
                nr' = foldr (`IM.insert` (val, n)) nr [col .. col' - 1]
             in aux col' (n + 1) rest (nr', ss)
        Nothing ->
            let (front, back) = C.span (== '.') s
                col' = col + C.length front
             in case C.uncons back of
                    Just (c, back') ->
                        if isDigit c
                            then aux col' n back (nr, ss)
                            else aux (col' + 1) n back' (nr, (c, (row, col')) : ss)
                    Nothing -> (n, (IM.insert row nr numMap, ss))

buildMap :: [ByteString] -> (NumberMap, Symbols)
buildMap = snd . L.foldl' (uncurry . buildMapRow) (0, (IM.empty, [])) . zip [0 ..]

checkIndices :: Int -> Int -> [(Int, Int)]
checkIndices row col =
    [ (row - 1, col - 1)
    , (row - 1, col)
    , (row - 1, col + 1)
    , (row, col - 1)
    , (row, col + 1)
    , (row + 1, col - 1)
    , (row + 1, col)
    , (row + 1, col + 1)
    ]

getNum :: NumberMap -> (Int, Int) -> Maybe (Int, Int)
getNum m (i, j) = (m !? i) >>= (!? j)

walkSymbols :: NumberMap -> Symbols -> Int
walkSymbols numMap =
    L.foldl' (\acc -> (acc +) . fst) 0
        . nubIntOn snd
        . L.foldl'
            ( \acc ->
                foldr (:) acc
                    . mapMaybe (getNum numMap)
                    . uncurry checkIndices
                    . snd
            )
            []

walkSymbols' :: NumberMap -> Symbols -> Int
walkSymbols' numMap =
    L.foldl' (+) 0
        . (L.foldl' (\acc -> (acc *) . fst) 1 . snd <$>)
        . filter (uncurry (&&) . ((== '*') *** ((== 2) . length)))
        . ((nubIntOn snd . mapMaybe (getNum numMap) . uncurry checkIndices <$>) <$>)
