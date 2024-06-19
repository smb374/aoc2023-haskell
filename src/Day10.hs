{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Day10 (Day10 (..)) where

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

data Day10 = Day10

instance Day Day10 where
    inputPath _ = "data/day10/input"

    part1 _ = fromMaybe 0 . (foldlPM' ((Just .) . flip (maybe <*> max)) Nothing <=< uncurry buildDistance) . parseInput
    part2 _ bs = case buildMainLoop pm dim of
        Just loop ->
            let dm = disjointReduce pm dim loop $ buildDisjointMap pm dim loop
                croots = disjointClosedRoots pm dim loop dm
                ps = L.filter (not . flip S.member loop) $ allPoints dim
             in L.length $ L.filter (isClosed dm croots) ps
        Nothing -> 0
      where
        (pm, dim) = parseInput bs

data Direction = N | S | W | E deriving (Show, Eq)

data Pipe
    = NS -- '|'
    | WE -- '-'
    | NW -- 'J'
    | NE -- 'L'
    | SW -- '7'
    | SE -- 'F'
    | Start -- 'S'
    deriving (Show, Eq)

type Dimension = (Int, Int)
type Point = (Int, Int)
type PointMap a = IntMap (IntMap a)
type PipeMap = PointMap Pipe
type DistMap = PointMap Int

data DisjointMap = DisjointMap
    { parents :: PointMap Point
    , ranks :: PointMap Int
    }
    deriving (Show, Eq)

maybeIf :: (a -> Bool) -> a -> Maybe a
maybeIf cond x = if cond x then Just x else Nothing

(@?) :: PointMap a -> Point -> Maybe a
pm @? (r, c) = pm !? r >>= \im -> im !? c

insertPM :: Point -> a -> PointMap a -> PointMap a
insertPM (r, c) v = IM.alter (Just . maybe (IM.singleton c v) (IM.insert c v)) r

memberPM :: Point -> PointMap a -> Bool
memberPM (r, c) = maybe False (IM.member c) . IM.lookup r

keysPM :: PointMap a -> [Point]
-- (. ((flip . ((:) .) . (,)) *** IM.keys)) . uncurry . flip L.foldl'
-- <-> \acc -> uncurry (`L.foldl'` acc) . ((flip . ((:) .) . (,)) *** IM.keys)
-- <-> \acc (r, cm) -> uncurry (`L.foldl'` acc) (flip ((:) . (r,)), IM.keys cm)
-- <-> \acc (r, cm) -> L.foldl' (flip ((:) . (r,))) acc $ IM.keys cm
keysPM = L.foldl' ((. ((flip . ((:) .) . (,)) *** IM.keys)) . uncurry . flip L.foldl') [] . IM.toList . IM.filter (not . IM.null)

filterPM :: (a -> Bool) -> PointMap a -> PointMap a
filterPM cond = L.foldl' (flip (IM.update (maybeIf (not . IM.null) . IM.filter cond))) <*> IM.keys

alterPM :: (Maybe a -> Maybe a) -> Point -> PointMap a -> PointMap a
alterPM f p@(r, c) pm = case f (pm @? p) of
    Just v -> IM.alter (Just . maybe (IM.singleton c v) (IM.alter (const (Just v)) c)) r pm
    Nothing -> IM.alter (maybeIf (not . IM.null) . IM.alter (const Nothing) c =<<) r pm

singletonPM :: Point -> a -> PointMap a
singletonPM (r, c) v = IM.singleton r (IM.singleton c v)

fromListPM :: [(Point, a)] -> PointMap a
fromListPM = L.foldl' (\m ((r, c), v) -> IM.alter (Just . maybe (IM.singleton c v) (IM.insert c v)) r m) IM.empty

foldlPM' :: (a -> b -> a) -> a -> PointMap b -> a
foldlPM' = IM.foldl' . IM.foldl'

elemsPM :: PointMap a -> [a]
elemsPM = foldlPM' (flip (:)) []

getDimension :: ByteString -> Dimension
getDimension = swap . maybe (0, 0) (C.length *** ((1 +) . L.length)) . L.uncons . C.lines

inBound :: Dimension -> Point -> Bool
inBound (rlim, clim) (r, c) = r >= 0 && r < rlim && c >= 0 && c < clim

adjacentLattice :: Point -> Dimension -> [Point]
adjacentLattice (r, c) dim = L.filter (inBound dim) [(r - 1, c), (r + 1, c), (r, c - 1), (r, c + 1)]

adjacentPipePoints :: PipeMap -> Dimension -> Point -> Set Point
adjacentPipePoints m dim p@(row, col) =
    maybe
        S.empty
        ( S.fromList . \case
            NS -> [(row - 1, col), (row + 1, col)]
            WE -> [(row, col - 1), (row, col + 1)]
            NW -> [(row - 1, col), (row, col - 1)]
            NE -> [(row - 1, col), (row, col + 1)]
            SW -> [(row + 1, col), (row, col - 1)]
            SE -> [(row + 1, col), (row, col + 1)]
            Start -> fst <$> L.filter (S.member p . snd) ((,) <*> adjacentPipePoints m dim <$> L.filter (`memberPM` m) ps)
        )
        $ m @? p
  where
    ps = adjacentLattice p dim

-- adjacentDirection a b =
-- Just {N,S,W,E} -> b is {N,S,W,E} of a
-- Nothing -> not adjacent
adjacentDirection :: Point -> Point -> Maybe Direction
adjacentDirection (r1, c1) (r2, c2)
    | c1 == c2 && r2 - r1 == 1 = Just N
    | c1 == c2 && r1 - r2 == 1 = Just S
    | r1 == r2 && c1 - c2 == 1 = Just W
    | r1 == r2 && c2 - c1 == 1 = Just E
    | otherwise = Nothing

allPoints :: Dimension -> [Point]
allPoints (r, c) = [(i, j) | i <- [0 .. r - 1], j <- [0 .. c - 1]]

isAdjacent :: Point -> Point -> Bool
isAdjacent (r1, c1) (r2, c2) = (abs (r1 - r2) == 1) /= (abs (c1 - c2) == 1)

parseLine :: Int -> ByteString -> PipeMap -> PipeMap
parseLine row = f 0
  where
    f col s m = case C.uncons s of
        Just (c, s') ->
            let p = (row, col)
                m' = case c of
                    '|' -> insertPM p NS m
                    '-' -> insertPM p WE m
                    'J' -> insertPM p NW m
                    'L' -> insertPM p NE m
                    '7' -> insertPM p SW m
                    'F' -> insertPM p SE m
                    'S' -> insertPM p Start m
                    _ -> m
             in f (col + 1) s' m'
        Nothing -> m

parseInput :: ByteString -> (PipeMap, Dimension)
parseInput = (,) <$> L.foldl' (flip (uncurry parseLine)) IM.empty . zip [0 ..] . C.lines <*> getDimension

-- Part 1
buildDistance :: PipeMap -> Dimension -> Maybe DistMap
buildDistance pm dim = builder . fst <$> L.uncons (keysPM $ filterPM (== Start) pm)
  where
    builder start = L.foldl' (flip (buildDistanceStep pm dim 1 start)) (singletonPM start 0) (adjacentPipePoints pm dim start)

buildDistanceStep :: PipeMap -> Dimension -> Int -> Point -> Point -> DistMap -> DistMap
buildDistanceStep pm dim n prev curr dm = case nextPs of
    [next] | Just Start /= pm @? next -> buildDistanceStep pm dim (n + 1) curr next $ alterPM (Just . (maybe <*> min) n) curr dm
    _ -> dm
  where
    nextPs = S.toList $ S.filter (/= prev) $ adjacentPipePoints pm dim curr

-- Part 2

-- Check if the points contain pipes and the pipes are connected, blocking open space.
isBlock :: PipeMap -> Point -> Point -> Bool
isBlock pm p1 p2 = case (,) <$> (pm @? p1) <*> (pm @? p2) of
    Just (pipe1, pipe2) -> case adjacentDirection p1 p2 of
        Just d -> case d of
            N -- p2 is north of p1
                | pipe1 == NS || pipe1 == SW || pipe1 == SE -> pipe2 == NS || pipe2 == NW || pipe2 == NE || pipe2 == Start
                | pipe1 == Start -> pipe2 == NS || pipe2 == NW || pipe2 == NE
            S -- p2 is south of p1
                | pipe1 == NS || pipe1 == NW || pipe1 == NE -> pipe2 == NS || pipe2 == SW || pipe2 == SE || pipe2 == Start
                | pipe1 == Start -> pipe2 == NS || pipe2 == SW || pipe2 == SE
            W -- p2 is west of p1
                | pipe1 == WE || pipe1 == NW || pipe1 == SW -> pipe2 == WE || pipe2 == NE || pipe2 == SE || pipe2 == Start
                | pipe1 == Start -> pipe2 == WE || pipe2 == NE || pipe2 == SE
            E -- p2 is east of p1
                | pipe1 == WE || pipe1 == NE || pipe1 == SE -> pipe2 == WE || pipe2 == NW || pipe2 == SW || pipe2 == Start
                | pipe1 == Start -> pipe2 == WE || pipe2 == NW || pipe2 == SW
            _ -> False
        Nothing -> False -- at least one of points is vacant
    Nothing -> False -- not adjacent

isBlockByLoop :: PipeMap -> Set Point -> Point -> Point -> Bool
isBlockByLoop pm loop p1 p2 = case (,) <$> (pm @? p1) <*> (pm @? p2) of
    Just (pipe1, pipe2) | S.member p1 loop && S.member p2 loop -> case adjacentDirection p1 p2 of
        Just d -> case d of
            N -- p2 is north of p1
                | pipe1 == NS || pipe1 == SW || pipe1 == SE -> pipe2 == NS || pipe2 == NW || pipe2 == NE || pipe2 == Start
                | pipe1 == Start -> pipe2 == NS || pipe2 == NW || pipe2 == NE
            S -- p2 is south of p1
                | pipe1 == NS || pipe1 == NW || pipe1 == NE -> pipe2 == NS || pipe2 == SW || pipe2 == SE || pipe2 == Start
                | pipe1 == Start -> pipe2 == NS || pipe2 == SW || pipe2 == SE
            W -- p2 is west of p1
                | pipe1 == WE || pipe1 == NW || pipe1 == SW -> pipe2 == WE || pipe2 == NE || pipe2 == SE || pipe2 == Start
                | pipe1 == Start -> pipe2 == WE || pipe2 == NE || pipe2 == SE
            E -- p2 is east of p1
                | pipe1 == WE || pipe1 == NE || pipe1 == SE -> pipe2 == WE || pipe2 == NW || pipe2 == SW || pipe2 == Start
                | pipe1 == Start -> pipe2 == WE || pipe2 == NW || pipe2 == SW
            _ -> False
        Nothing -> False -- not adjacent
    _ -> False -- at least one of points is vacant

buildClosedLoop :: PipeMap -> Dimension -> Point -> Maybe (Set Point)
buildClosedLoop pm dim p = case S.toList $ adjacentPipePoints pm dim p of
    [p1, p2] -> (flip (walk (== p2)) p1 <*> S.singleton) p
    _ -> Nothing
  where
    walk cond prev curr set =
        if cond curr
            then Just (S.insert curr set)
            else case L.filter ((&&) <$> (/= prev) <*> isBlock pm curr) $ S.toList $ adjacentPipePoints pm dim curr of
                [next] -> walk cond curr next (S.insert curr set)
                _ -> Nothing

buildMainLoop :: PipeMap -> Dimension -> Maybe (Set Point)
buildMainLoop pm dim = buildClosedLoop pm dim . fst =<< L.uncons (keysPM $ filterPM (== Start) pm)

-- Spaces on axis multiplied by 2
-- Points with both x & y are even are vertices on the original map.
allSpaces :: Dimension -> [Point]
allSpaces (rlim, clim) = filter (not . uncurry (&&) . (even *** even)) ps
  where
    ps = [(i, j) | i <- [0 .. shiftL rlim 1 - 1], j <- [0 .. shiftL clim 1 - 1]]

isOpenSpace :: PipeMap -> Set Point -> Point -> Bool
isOpenSpace pm loop (r, c) =
    case (even r, even c) of
        (True, False) -> not $ isBlockByLoop pm loop (r', cw) (r', ce) -- horizontal space
        (False, True) -> not $ isBlockByLoop pm loop (rn, c') (rs, c') -- vertical space
        (False, False) ->
            -- lattice space, blocked only if all adjacent horizontal & vertical spaces are blocked.
            not
                ( isBlockByLoop pm loop (rn, cw) (rn, ce)
                    && isBlockByLoop pm loop (rs, cw) (rs, ce)
                    && isBlockByLoop pm loop (rn, cw) (rs, cw)
                    && isBlockByLoop pm loop (rn, ce) (rs, ce)
                )
        _ -> False
  where
    r' = shiftR r 1
    c' = shiftR c 1
    rn = shiftR (r - 1) 1
    rs = shiftR (r + 1) 1
    cw = shiftR (c - 1) 1
    ce = shiftR (c + 1) 1

openSpaces :: PipeMap -> Dimension -> Set Point -> [Point]
openSpaces pm dim loop = filter (isOpenSpace pm loop) ss
  where
    ss = allSpaces dim

openSpacesRowMajor :: PipeMap -> Dimension -> Set Point -> [[Point]]
openSpacesRowMajor pm (rlim, clim) loop = filter (isOpenSpace pm loop) <$> ss
  where
    ss = filter (not . uncurry (&&) . (even *** even)) . (\r -> (r,) <$> [0 .. shiftL clim 1 - 1]) <$> [0 .. shiftL rlim 1 - 1]

openSpacesColumnMajor :: PipeMap -> Dimension -> Set Point -> [[Point]]
openSpacesColumnMajor pm (rlim, clim) loop = filter (isOpenSpace pm loop) <$> ss
  where
    ss = filter (not . uncurry (&&) . (even *** even)) . (\c -> (,c) <$> [0 .. shiftL rlim 1 - 1]) <$> [0 .. shiftL clim 1 - 1]

boundaryOpenSpaces :: PipeMap -> Dimension -> Set Point -> [Point]
boundaryOpenSpaces pm (rlim, clim) loop = filter (isOpenSpace pm loop) ss
  where
    ss =
        filter (not . uncurry (&&) . (even *** even)) $
            [(0, j) | j <- [0 .. shiftL clim 1 - 1]]
                <> [(shiftL rlim 1 - 1, j) | j <- [0 .. shiftL clim 1 - 1]]
                <> [(i, 0) | i <- [0 .. shiftL rlim 1 - 1]]
                <> [(i, shiftL clim 1 - 1) | i <- [0 .. shiftL rlim 1 - 1]]

buildDisjointMap :: PipeMap -> Dimension -> Set Point -> DisjointMap
buildDisjointMap pm dim loop = DisjointMap pointMap rankMap
  where
    spaces = openSpaces pm dim loop
    pointMap = fromListPM $ zip spaces spaces
    rankMap = fromListPM $ (,0) <$> spaces

alterParents :: DisjointMap -> (Maybe Point -> Maybe Point) -> Point -> DisjointMap
alterParents (DisjointMap ps rs) f p = DisjointMap{parents = alterPM f p ps, ranks = rs}

alterRanks :: DisjointMap -> (Maybe Int -> Maybe Int) -> Point -> DisjointMap
alterRanks (DisjointMap ps rs) f p = DisjointMap{parents = ps, ranks = alterPM f p rs}

disjointFind :: DisjointMap -> Point -> Maybe (DisjointMap, Point)
disjointFind dm p =
    parents dm @? p
        >>= \q ->
            if p == q
                then Just (dm, q)
                else disjointFind dm q >>= \(dm', r) -> Just (alterParents dm' (const (Just r)) p, r)

disjointLink :: DisjointMap -> Point -> Point -> Maybe DisjointMap
disjointLink dm p q =
    (,) <$> (ranks dm @? p) <*> (ranks dm @? q) >>= \(rp, rq) ->
        if rp > rq
            then Just $ alterParents dm (const (parents dm @? p)) q
            else
                let dm' = alterParents dm (const (parents dm @? q)) p
                 in Just $
                        if rp == rq
                            then alterRanks dm' (const (Just $ rq + 1)) q
                            else dm'

disjointUnion :: DisjointMap -> Point -> Point -> Maybe DisjointMap
disjointUnion dm p q =
    disjointFind dm p
        >>= \(dm1, pp) ->
            disjointFind dm1 q
                >>= \(dm2, pq) ->
                    disjointLink dm2 pp pq

disjointReduce :: PipeMap -> Dimension -> Set Point -> DisjointMap -> DisjointMap
-- (S.foldl' (((.) . flip maybe fst) <*> disjointFind) <*> disjointParents) . layerScan cols . layerScan rows
-- <-> \dm -> (S.foldl' (((.) . flip maybe fst) <*> disjointFind) <*> disjointParents) $ layerScan cols (layerScan rows dm)
-- <-> \dm -> let dm' = layerScan cols (layerScan rows dm) in S.foldl' (((.) . flip maybe fst) <*> disjointFind) dm' (disjointParents dm')
-- <-> \dm -> let dm' = layerScan cols (layerScan rows dm) in S.foldl' (\m -> ((.) . flip maybe fst) m . disjointFind m) dm' (disjointParents dm')
-- <-> \dm -> let dm' = layerScan cols (layerScan rows dm) in S.foldl' (\m -> maybe m fst . disjointFind m) dm' (disjointParents dm')
-- <-> \dm -> let dm' = layerScan cols (layerScan rows dm) in S.foldl' (\m p -> maybe m fst $ disjointFind m p) dm' (disjointParents dm')
disjointReduce pm dim loop = (S.foldl' (((.) . flip maybe fst) <*> disjointFind) <*> disjointParents) . layerScan cols . layerScan rows
  where
    rows = openSpacesRowMajor pm dim loop
    cols = openSpacesColumnMajor pm dim loop
    -- flip (L.foldl' ((. (zip <*> tail)) . L.foldl' scanStep))
    -- <-> \ps dm -> L.foldl' ((. (zip <*> tail)) . L.foldl' scanStep) dm ps
    -- <-> \ps dm -> L.foldl' (\acc -> L.foldl' scanStep acc . (zip <*> tail)) dm ps
    -- <-> \ps dm -> L.foldl' (\acc p -> L.foldl' scanStep acc $ zip p (tail p)) dm ps
    layerScan = flip (L.foldl' ((. (zip <*> tail)) . L.foldl' scanStep))
    scanStep dm (p1, p2)
        | isAdjacent p1 p2 = fromMaybe dm $ disjointUnion dm p1 p2
        | otherwise = dm

disjointParents :: DisjointMap -> Set Point
disjointParents = S.fromList . elemsPM . parents

disjointRoots :: DisjointMap -> Set Point
disjointRoots dm = S.fromList $ S.foldl' (\acc -> maybe acc ((: acc) . snd) . disjointFind dm) [] $ disjointParents dm

findRoot :: DisjointMap -> Point -> Maybe Point
findRoot = curry (uncurry go . second Just)
  where
    go m point
        | point == point' = point
        | otherwise = go m point'
      where
        point' = point >>= (parents m @?)

disjointOpenRoots :: PipeMap -> Dimension -> Set Point -> DisjointMap -> Set Point
disjointOpenRoots pm dim loop = S.fromList . flip mapMaybe (boundaryOpenSpaces pm dim loop) . findRoot

disjointClosedRoots :: PipeMap -> Dimension -> Set Point -> DisjointMap -> Set Point
disjointClosedRoots pm dim loop = S.difference <$> disjointRoots <*> disjointOpenRoots pm dim loop

isClosed :: DisjointMap -> Set Point -> Point -> Bool
isClosed dm croots = maybe False (`S.member` croots) . findRoot dm . ((`shiftL` 1) *** (subtract 1 . (`shiftL` 1)))
