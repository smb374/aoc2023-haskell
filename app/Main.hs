module Main (main) where

import qualified Data.ByteString.Char8 as C
import Lib (intToFuncs)
import Options.Applicative
import Text.Printf (printf)

newtype Opts = Opts {day :: Int} deriving (Eq, Show)

opts :: Parser Opts
opts =
    Opts
        <$> option
            auto
            ( long "day"
                <> short 'd'
                <> help "Day to solve in Advent of Code 2023"
                <> metavar "DAY"
            )

process :: Int -> IO ()
process d = case intToFuncs d of
    Just (p, p1, p2) ->
        C.readFile p >>= \input ->
            putStrLn $
                printf "part1 = %d\npart2 = %d\n" (p1 input) (p2 input)
    Nothing -> putStrLn $ printf "Day %02d is not implemented." d

main :: IO ()
main = process . day =<< execParser opts'
  where
    opts' =
        info
            (opts <**> helper)
            ( fullDesc
                <> progDesc "Solve specific day for Advent of Code 2023"
                <> header "aoc2023-haskell: Advent of Code 2023 solver in Haskell"
            )
