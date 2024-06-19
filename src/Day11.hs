module Day11 where

import Classes (Day (..))

import Control.Arrow (Arrow (second), (***))
import Control.Monad ((<=<))
import Data.Bits (shiftL, shiftR)
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.IntMap.Strict (IntMap, (!?))
import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Tuple (swap)

data Day11 = Day11

instance Day Day11 where
    inputPath _ = "data/day11/input"

    part1 _ = const 0
    part2 _ = const 1
