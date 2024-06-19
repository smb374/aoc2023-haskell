{-# LANGUAGE LambdaCase #-}

module Day07 (Day07 (..)) where

import Classes (Day (..))
import Control.Arrow (Arrow (second))
import Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as C
import Data.Char (digitToInt, isDigit)
import qualified Data.List as L
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Ord (comparing)
import qualified Data.Set as S

data Day07 = Day07

instance Day Day07 where
    inputPath _ = "data/day07/input"
    part1 _ = solve False
    part2 _ = solve True

data CardType
    = FiveKind
    | FourKind
    | FullHouse
    | ThreeKind
    | TwoPair
    | OnePair
    | HighCard
    deriving (Show, Eq)

instance Ord CardType where
    compare FiveKind = \case
        FiveKind -> EQ
        _ -> GT
    compare HighCard = \case
        HighCard -> EQ
        _ -> LT
    compare FourKind = \case
        FiveKind -> LT
        FourKind -> EQ
        _ -> GT
    compare OnePair = \case
        HighCard -> GT
        OnePair -> EQ
        _ -> LT
    compare FullHouse = \case
        FiveKind -> LT
        FourKind -> LT
        FullHouse -> EQ
        _ -> GT
    compare TwoPair = \case
        HighCard -> GT
        OnePair -> GT
        TwoPair -> EQ
        _ -> LT
    compare ThreeKind = \case
        FiveKind -> LT
        FourKind -> LT
        FullHouse -> LT
        ThreeKind -> EQ
        _ -> GT

data Card
    = Card
    { cardType :: CardType
    , cardContent :: String
    , cardBid :: Int
    , jokerEnable :: Bool
    }
    deriving (Show, Eq)

instance Ord Card where
    compare c1 c2 = case compare (cardType c1) (cardType c2) of
        GT -> GT
        LT -> LT
        EQ -> cmp' $ zip (cardContent c1) (cardContent c2)
      where
        cmp' :: [(Char, Char)] -> Ordering
        cmp' [] = EQ
        cmp' ((x, y) : xs) = case compare (f x) (f y) of
            EQ -> cmp' xs
            r -> r
        f = charToValue (jokerEnable c1 || jokerEnable c2)

solve :: Bool -> ByteString -> Int
solve jEnable = maybe 0 (L.foldl' foldF 0 . zip [1 ..] . L.sort) . mapM (parseCard jEnable) . C.lines
  where
    foldF acc = (+ acc) . uncurry (*) . second cardBid

charSet :: S.Set Char
charSet = S.fromList "AKQJT98765432"

charToValue :: Bool -> Char -> Maybe Int
charToValue jEnable c =
    if S.member c charSet
        then
            if isDigit c
                then Just $ digitToInt c
                else case c of
                    'T' -> Just 10
                    'J' -> if jEnable then Just 1 else Just 11
                    'Q' -> Just 12
                    'K' -> Just 13
                    'A' -> Just 14
                    _ -> Nothing -- Impossible case, placed 'Nothing' for completeness
        else Nothing

charFreq :: ByteString -> Map Char Int
charFreq = flip charFreq' M.empty . C.unpack
  where
    charFreq' :: String -> Map Char Int -> Map Char Int
    charFreq' [] = id
    charFreq' (x : xs) = charFreq' xs . M.alter (Just . maybe 1 (+ 1)) x

parseCardType :: Bool -> ByteString -> Maybe CardType
parseCardType jEnable bs = case jcount of
    5 -> Just FiveKind
    4 -> Just FiveKind
    3 -> case freq of
        [(_, 2)] -> Just FiveKind
        [(_, 1), (_, 1)] -> Just FourKind
        _ -> Nothing
    2 -> case freq of
        [(_, 3)] -> Just FiveKind
        [(_, 1), (_, 2)] -> Just FourKind
        [(_, 1), (_, 1), (_, 1)] -> Just ThreeKind
        _ -> Nothing
    1 -> case freq of
        [(_, 4)] -> Just FiveKind
        [(_, 1), (_, 3)] -> Just FourKind
        [(_, 2), (_, 2)] -> Just FullHouse
        [(_, 1), (_, 1), (_, 2)] -> Just ThreeKind
        [(_, 1), (_, 1), (_, 1), (_, 1)] -> Just OnePair
        _ -> Nothing
    0 -> case freq of
        [(_, 5)] -> Just FiveKind
        [(_, 1), (_, 4)] -> Just FourKind
        [(_, 2), (_, 3)] -> Just FullHouse
        [(_, 1), (_, 1), (_, 3)] -> Just ThreeKind
        [(_, 1), (_, 2), (_, 2)] -> Just TwoPair
        [(_, 1), (_, 1), (_, 1), (_, 2)] -> Just OnePair
        [(_, 1), (_, 1), (_, 1), (_, 1), (_, 1)] -> Just HighCard
        _ -> Nothing
    _ -> Nothing
  where
    freqMap = charFreq bs
    (jcount, freqList) =
        if jEnable
            then (fromMaybe 0 $ M.lookup 'J' freqMap, M.toList $ M.delete 'J' freqMap)
            else (0, M.toList freqMap)
    freq = L.sortBy (comparing snd) freqList

parseCard :: Bool -> ByteString -> Maybe Card
parseCard jEnable bs = case C.split ' ' bs of
    [cardVal, bidVal] ->
        (,) <$> parseCardType jEnable cardVal <*> C.readInt bidVal
            >>= \(ctype, (bid, _)) ->
                return
                    Card
                        { cardType = ctype
                        , cardContent = C.unpack bs
                        , cardBid = bid
                        , jokerEnable = jEnable
                        }
    _ -> Nothing
