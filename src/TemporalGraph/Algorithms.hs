module TemporalGraph.Algorithms where

import Definitions

import TemporalGraph.Graph

import Data.Array
import Data.Array.ST
import Control.Monad.ST

-- Functions for Earliest Arrival Time and Latest Departure Time. 

type TemporalSet s = STArray s Index (Maybe Time)

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

earliestArrivalTime :: TemporalGraph -> Index -> TimeInterval -> Table (Maybe Time)
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
        getValue m v1 >>= \t1 ->
            if (t + l) `maybeLTE` t1 then
                getValue m v2 >>= \t2 ->
                    if t `maybeGT` t2 then
                        setValue m v2 t >>= \_ -> return m
                    else
                        return m
            else
                return m

latestDepartureTime :: TemporalGraph -> Index -> TimeInterval -> Table (Maybe Time)
latestDepartureTime g v (t1, t2)= 
    runSTArray (
        edgeStreamFoldr
            (trimByLesserValue (getEdgeStream g) t1)
            latestDepartureAux
            (getInitialState (bounds g) v t2)
    )

type TimeDuple = (Time, Time)
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
          get (s, a) m = if a <= t then (Just (s, a)) else m

removeDominated :: (TimeDuple -> TimeDuple -> Bool) -> TimeDuple -> VertexList -> VertexList
removeDominated f td lv = foldr remove [] lv
    where remove :: (TimeDuple) -> VertexList -> VertexList
          remove td' ls = 
            if f td td' then
                ls
            else
                td':ls

dominatesS :: TimeDuple -> TimeDuple -> Bool
dominatesS (s1, a1) (s2, a2) = (s1 > s2 && a1 <= a2) || (s1 == s2 && a1 < a2)

insertDupleS :: TimeDuple -> VertexList -> VertexList
insertDupleS (s1, a1) [] = [(s1, a1)]
insertDupleS (s1, a1) ((s2, a2):ls)
    | a2 < a1 = (s1, a1) : ((s2, a2):ls)
    | otherwise = (s2, a2) : insertDupleS (s1, a1) ls

modifyDupleS :: Time -> Time -> VertexList -> VertexList
modifyDupleS s a lv = foldr insert [] lv
    where insert :: (TimeDuple) -> VertexList -> VertexList
          insert (s', a') lv =
            if s == s' then
                (s, a) : lv
            else
                (s', a') : lv

containsS :: Time -> VertexList -> Bool
containsS t lv = foldr (cont t) False lv
    where cont :: Time -> (TimeDuple) -> Bool -> Bool
          cont t (s, a) _ = t == s

edgeStreamFoldrVL :: [TemporalEdge] -> FoldrVLStateFunc s -> ST s (VertexListSet s) -> ST s (VertexListSet s)
edgeStreamFoldrVL edgeStream f initialState = foldr f initialState edgeStream

getTimeTable :: Table (Maybe Time, VertexList) -> Table (Maybe Time)
getTimeTable = mapT getTime
    where getTime _ (t, _) = t

fastestPathDurationAux :: Index -> FoldrVLStateFunc s
fastestPathDurationAux i (v1, v2, (t, l)) set =
    set >>= \m ->
    if i == v1 then
        insertDupleSVL m v1 (t, t) >>= \_ -> fpd m v1 v2 (t, l)
    else
        fpd m v1 v2 (t, l)
    where fpd :: VertexListSet s -> Index -> Index -> TimeInterval -> ST s (VertexListSet s)
          fpd m u v (t, l) = getValueVL m u >>= \(mt, lu) -> maxmaybe (getMaximumBeforeTime lu t) m v (t, l)
          maxmaybe :: Maybe TimeDuple -> VertexListSet s -> Index -> TimeInterval -> ST s (VertexListSet s)
          maxmaybe Nothing m _ _ = return m
          maxmaybe (Just (s, a)) m v (t, l) = 
            getValueVL m v >>= \(_, vl) ->
            if containsS s vl then
                insertVL m v (removeDominated dominatesS (s, t + l) (modifyDupleS s (t + l) vl)) >>= \_ -> setTime (s, t + l) m v
            else
                insertVL m v (removeDominated dominatesS (s, t + l) (insertDupleS (s, t + l) vl)) >>= \_ -> setTime (s, t + l) m v
          setTime :: TimeDuple -> VertexListSet s -> Index -> ST s (VertexListSet s)
          setTime (s, a) m v =
            getValueVL m v >>= \(t, _) ->
            if (a - s) `maybeLT` t then
                insertF m v (a - s) >>= \_ -> return m
            else
                return m
         
fastestPathDuration :: TemporalGraph -> Index -> TimeInterval -> Table (Maybe Time)
fastestPathDuration g v (t1, t2) = 
    getTimeTable (
        runSTArray (
            edgeStreamFoldrVL 
                (trimByInterval (getEdgeStream g) (t1, t2))
                (fastestPathDurationAux v)
                (getInitialStateVL (bounds g) v)
        )
    )

dominatesD :: TimeDuple -> TimeDuple -> Bool
dominatesD (d1, a1) (d2, a2) = (d1 < d2 && a1 <= a2) || (d1 == d2 && a1 < a2)

insertDupleD :: TimeDuple -> VertexList -> VertexList
insertDupleD (d1, a1) [] = [(d1, a1)]
insertDupleD (d1, a1) ((d2, a2):ls)
    | a2 < a1 = (d1, a1) : ((d2, a2):ls)
    | otherwise = (d2, a2) : insertDupleD (d1, a1) ls

modifyDupleD :: Time -> Time -> VertexList -> VertexList
modifyDupleD d a lv = foldr insert [] lv
    where insert :: (TimeDuple) -> VertexList -> VertexList
          insert (d', a') lv =
            if a == a' then
                (d, a) : lv
            else
                (d', a') : lv

containsD :: Time -> VertexList -> Bool
containsD t lv = foldr (cont t) False lv
    where cont :: Time -> (TimeDuple) -> Bool -> Bool
          cont t (d, a) _ = t == a

-- shortestPathDurationAux :: Index -> FoldrVLStateFunc s
shortestPathDurationAux i (v1, v2, (t, l)) set =
    set >>= \m ->
    if i == v1 then
        insertDupleSVL m v1 (0, t) >>= \_ -> fpd m v1 v2 (t, l)
    else
        fpd m v1 v2 (t, l)
    where fpd :: VertexListSet s -> Index -> Index -> TimeInterval -> ST s (VertexListSet s)
          fpd m u v (t, l) = getValueVL m u >>= \(mt, lu) -> maxmaybe (getMaximumBeforeTime lu t) m v (t, l)
          maxmaybe :: Maybe TimeDuple -> VertexListSet s -> Index -> TimeInterval -> ST s (VertexListSet s)
          maxmaybe Nothing m v (t, l) = return m
          maxmaybe (Just (d, a)) m v (t, l) = 
            getValueVL m v >>= \(_, vl) ->
            if containsD (t + l) vl then
                insertVL m v (removeDominated dominatesD (d + 1, t + l) (modifyDupleD (d + 1) (t + l) vl)) >>= \_ -> setDistance (d + 1, t + l) m v
            else
                insertVL m v (removeDominated dominatesD (d + 1, t + l) (insertDupleD (d + 1, t + l) vl)) >>= \_ -> setDistance (d + 1, t + l) m v
          setDistance :: TimeDuple -> VertexListSet s -> Index -> ST s (VertexListSet s)
          setDistance (d, a) m v =
            getValueVL m v >>= \(t, _) ->
            if d `maybeLT` t then
                insertF m v d >>= \_ -> return m
            else
                return m
         
shortestPathDuration :: TemporalGraph -> Index -> TimeInterval -> Table (Maybe Time)
shortestPathDuration g v (t1, t2) = 
    getTimeTable (
        runSTArray (
            edgeStreamFoldrVL 
                (trimByInterval (getEdgeStream g) (t1, t2))
                (shortestPathDurationAux v)
                (getInitialStateVL (bounds g) v)
        )
    )