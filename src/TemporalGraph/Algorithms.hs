module TemporalGraph.Algorithms where

import Definitions

import TemporalGraph.Graph

import Data.Array
import Data.Array.ST
import Control.Monad.ST

-- Functions for Earliest Arrival Time and Latest Departure Time. 

type TemporalSet s = STArray s Index (Maybe Time)
type TimeAlgorithm = TemporalGraph -> Index -> TimeInterval -> Table (Maybe Int)

getEmptyState :: Bounds -> ST s (TemporalSet s)
getEmptyState bnds = newArray bnds Nothing

getValue :: TemporalSet s -> Index -> ST s (Maybe Time)
getValue m v = readArray m v

setValue :: TemporalSet s -> Index -> Time -> ST s ()
setValue m v t = writeArray m v (Just t)

getInitialState :: Bounds -> Index -> Time -> ST s (TemporalSet s)
getInitialState bnds v t = getEmptyState bnds >>= \m -> setValue m v t >>= \_ -> return m

maybeLT :: Time -> (Maybe Time) -> Bool
maybeLT _ Nothing = True
maybeLT t1 (Just t2) = t1 < t2

maybeLTE :: Time -> (Maybe Time) -> Bool
maybeLTE _ Nothing = False
maybeLTE t1 (Just t2) = t1 <= t2

maybeGT :: Time -> (Maybe Time) -> Bool
maybeGT _ Nothing = True
maybeGT t1 (Just t2) = t1 > t2

maybeGTE :: Time -> (Maybe Time) -> Bool
maybeGTE _ Nothing = False
maybeGTE t1 (Just t2) = t1 >= t2

type FoldrStateFunc s = TemporalEdge -> ST s (TemporalSet s) -> ST s (TemporalSet s)

edgeStreamFoldr :: [TemporalEdge] -> FoldrStateFunc s -> ST s (TemporalSet s) -> ST s (TemporalSet s)
edgeStreamFoldr edgeStream f initialState = foldr f initialState edgeStream

earliestArrivalAux :: FoldrStateFunc s
earliestArrivalAux (v1, v2, (t, l)) set = 
    set >>= \m ->
        getValue m v1 >>= \t1 ->
            if t `maybeGTE` t1 then
                getValue m v2 >>= \t2 ->
                    if (t + l) `maybeLT` t2 then
                        setValue m v2 (t + l) >>= \_ -> return m
                    else
                        return m
            else
                return m

earliestArrivalTime :: TimeAlgorithm
earliestArrivalTime g v (t1, t2)= 
    runSTArray (
        edgeStreamFoldr
            (trimByGreaterValue (getEdgeStream g) t2)
            earliestArrivalAux
            (getInitialState (bounds g) v t1)
    )

latestDepartureAux :: FoldrStateFunc s
latestDepartureAux (v1, v2, (t, l)) set = 
    set >>= \m ->
        getValue m v2 >>= \t2 ->
            if (t + l) `maybeLTE` t2 then
                getValue m v1 >>= \t1 ->
                    if t `maybeGT` t1 then
                        setValue m v1 t >>= \_ -> return m
                    else
                        return m
            else
                return m

latestDepartureTime :: TimeAlgorithm
latestDepartureTime g v (t1, t2)= 
    runSTArray (
        edgeStreamFoldr
            (trimByLesserValue (getReverseEdgeStream g) t1)
            latestDepartureAux
            (getInitialState (bounds g) v t2)
    )

type TimeDuple = (Int, Int)
type VertexList = [(TimeDuple)]
type VertexListSet s = STArray s Index ((Maybe Time), VertexList)
type FoldrVLStateFunc s = TemporalEdge -> ST s (VertexListSet s) -> ST s (VertexListSet s)

getEmptyStateVL :: Bounds -> ST s (VertexListSet s)
getEmptyStateVL bnds = newArray bnds (Nothing, [])

getValueVL :: VertexListSet s -> Index -> ST s ((Maybe Time), VertexList)
getValueVL m v = readArray m v

setValueVL :: VertexListSet s -> Index -> Time -> VertexList -> ST s ()
setValueVL m v t vl = writeArray m v ((Just t), vl)

setValueRawVL :: VertexListSet s -> Index -> (Maybe Time) -> VertexList -> ST s ()
setValueRawVL m v t vl = writeArray m v (t, vl)

getInitialStateVL :: Bounds -> Index -> ST s (VertexListSet s)
getInitialStateVL bnds v = getEmptyStateVL bnds >>= \m -> setValueVL m v 0 [] >>= \_ -> return m

insertDupleSVL :: VertexListSet s -> Index -> TimeDuple  -> ST s ()
insertDupleSVL m v d = getValueVL m v >>= \(t, lv) -> setValueRawVL m v t (insertDupleS d lv)

insertVL :: VertexListSet s -> Index -> VertexList  -> ST s ()
insertVL m v lv = getValueVL m v >>= \(t, _) -> setValueRawVL m v t lv

insertF :: VertexListSet s -> Index -> Time  -> ST s ()
insertF m v t = getValueVL m v >>= \(_, lv) -> setValueVL m v t lv

getMaximumBeforeTime :: VertexList -> Time -> Maybe TimeDuple
getMaximumBeforeTime lv t = foldr get Nothing lv
    where get :: TimeDuple -> Maybe TimeDuple -> Maybe TimeDuple
          get (s, a) Nothing = if a <= t then (Just (s, a)) else Nothing
          get (s, a) (Just (s', a')) = if a <= t && a > a' then (Just (s, a)) else Just (s', a')

removeDominated :: (DominationFunc) -> TimeDuple -> VertexList -> VertexList
removeDominated f td lv = foldr remove [] lv
    where remove :: (TimeDuple) -> VertexList -> VertexList
          remove td' ls = 
            if f td td' then
                ls
            else
                td':ls

type DominationFunc = TimeDuple -> TimeDuple -> Bool
type ResultSetter s = TimeDuple -> VertexListSet s -> Index -> ST s (VertexListSet s)
type DupleModifier = Time -> Time -> VertexList -> VertexList
type DupleInserter = TimeDuple -> VertexList -> VertexList
type DupleContainer = Time -> VertexList -> Bool

dominatesS :: DominationFunc
dominatesS (s1, a1) (s2, a2) = (s1 > s2 && a1 <= a2) || (s1 == s2 && a1 < a2)

insertDupleS :: DupleInserter
insertDupleS (s1, a1) [] = [(s1, a1)]
insertDupleS (s1, a1) ((s2, a2):ls)
    | a2 < a1 = (s1, a1) : ((s2, a2):ls)
    | otherwise = (s2, a2) : insertDupleS (s1, a1) ls

modifyDupleS :: DupleModifier
modifyDupleS s a lv = foldr insert [] lv
    where insert :: (TimeDuple) -> VertexList -> VertexList
          insert (s', a') lv =
            if s == s' then
                (s, a) : lv
            else
                (s', a') : lv

containsS :: DupleContainer
containsS t lv = foldr (cont t) False lv
    where cont :: Time -> (TimeDuple) -> Bool -> Bool
          cont t (s, a) _ = t == s

dominatesD :: DominationFunc
dominatesD (d1, a1) (d2, a2) = ((d1 + 1) < d2 && a1 <= a2) || ((d1 + 1) == d2 && a1 < a2)

insertDupleD :: DupleInserter
insertDupleD (d1, a1) [] = [(d1 + 1, a1)]
insertDupleD (d1, a1) ((d2, a2):ls)
    | a2 < a1 = (d1 + 1, a1) : ((d2, a2):ls)
    | otherwise = (d2, a2) : insertDupleD (d1, a1) ls

modifyDupleD :: DupleModifier
modifyDupleD d a lv = foldr insert [] lv
    where insert :: (TimeDuple) -> VertexList -> VertexList
          insert (d', a') lv =
            if a == a' then
                (d + 1, a) : lv
            else
                (d', a') : lv

containsD :: DupleContainer
containsD t lv = foldr (cont t) False lv
    where cont :: Time -> (TimeDuple) -> Bool -> Bool
          cont t (d, a) _ = t == a

edgeStreamFoldrVL :: [TemporalEdge] -> FoldrVLStateFunc s -> ST s (VertexListSet s) -> ST s (VertexListSet s)
edgeStreamFoldrVL edgeStream f initialState = foldr f initialState edgeStream

getTimeTable :: Table (Maybe Time, VertexList) -> Table (Maybe Time)
getTimeTable = mapT getTime
    where getTime _ (t, _) = t

setTime :: ResultSetter s
setTime (s, a) m v =
    getValueVL m v >>= \(t, _) ->
    if (a - s) `maybeLT` t then do
        _ <- insertF m v (a - s)
        return m
    else
        return m

setDistance :: ResultSetter s
setDistance (d, a) m v =
    getValueVL m v >>= \(t, _) ->
    if (d + 1) `maybeLT` t then do
        _ <- insertF m v (d + 1)
        return m
    else
        return m

dominationAux :: Index -> Int -> DominationFunc -> ResultSetter s -> DupleModifier -> DupleInserter -> DupleContainer -> FoldrVLStateFunc s
dominationAux i initialState domFunc setter modifier inserter container (v1, v2, (t, l)) set =
    set >>= \m ->
    if i == v1 then do
        _ <- insertDupleSVL m v1 (initialState, t)
        fpd m setter v1 v2 (t, l)
    else
        fpd m setter v1 v2 (t, l)
    where fpd :: VertexListSet s -> ResultSetter s -> Index -> Index -> TimeInterval -> ST s (VertexListSet s)
          fpd m setter u v (t, l) = 
              do
                (mt, lu) <- getValueVL m u
                maxmaybe (getMaximumBeforeTime lu t) setter m v (t, l)
          maxmaybe :: Maybe TimeDuple -> ResultSetter s -> VertexListSet s -> Index -> TimeInterval -> ST s (VertexListSet s)
          maxmaybe Nothing _ m _ _ = return m
          maxmaybe (Just (s, a)) setter m v (t, l) = 
            do
                (_, vl) <- getValueVL m v
                if container s vl then do
                    _ <- insertVL m v (removeDominated domFunc (s, t + l) (modifier s (t + l) vl))
                    setter (s, t + l) m v
                else do
                    _ <- insertVL m v (removeDominated domFunc (s, t + l) (inserter (s, t + l) vl))
                    setter (s, t + l) m v

fastestPathDurationAux :: Index -> FoldrVLStateFunc s
fastestPathDurationAux i (v1, v2, (t, l)) set = 
    dominationAux i t dominatesS setTime modifyDupleS insertDupleS containsS (v1, v2, (t, l)) set

shortestPathDurationAux :: Index -> FoldrVLStateFunc s
shortestPathDurationAux i interval set = 
    dominationAux i 0 dominatesD setDistance modifyDupleD insertDupleD containsD interval set
         
fastestPathDuration :: TimeAlgorithm
fastestPathDuration g v (t1, t2) = 
    getTimeTable (
        runSTArray (
            edgeStreamFoldrVL 
                (trimByInterval (getEdgeStream g) (t1, t2))
                (fastestPathDurationAux v)
                (getInitialStateVL (bounds g) v)
        )
    )
         
shortestPathDuration :: TimeAlgorithm
shortestPathDuration g v (t1, t2) = 
    getTimeTable (
        runSTArray (
            edgeStreamFoldrVL 
                (trimByInterval (getEdgeStream g) (t1, t2))
                (shortestPathDurationAux v)
                (getInitialStateVL (bounds g) v)
        )
    )

algorithms = [
    ("shortestPath", (shortestPathDuration, "Shortest Path Length")),
    ("latestDeparturePath", (latestDepartureTime, "Latest Departure Time")),
    ("earliestArrivalPath", (earliestArrivalTime, "Earliest Departure Time")),
    ("fastestPath", (fastestPathDuration, "Fastest Path Time"))
    ]

getAlgorithm :: String -> Maybe (TimeAlgorithm, String)
getAlgorithm str = foldr (f str) Nothing algorithms
    where f :: String -> (String, (TimeAlgorithm, String)) -> Maybe (TimeAlgorithm, String) -> Maybe (TimeAlgorithm, String)
          f str (name, func) z
            | name == str = Just func
            | otherwise = z
