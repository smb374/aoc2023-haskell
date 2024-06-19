module Classes (Day (..)) where

import Data.ByteString (ByteString)

class Day a where
    inputPath :: a -> FilePath
    part1 :: a -> (ByteString -> Int)
    part2 :: a -> (ByteString -> Int)
