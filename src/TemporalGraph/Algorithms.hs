module TemporalGraph.Algorithms where

import Definitions

import TemporalGraph.Graph

import Data.Array
import Data.Array.ST
import Control.Monad.ST

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
            if maybeGTE t t1 then
                getValue m v2 >>= \t2 ->
                    if maybeLT (t + l) t2 then
                        setValue m v2 (t + l) >>= \_ -> return m
                    else
                        return m
            else
                return m

earliestArrivalTime :: TemporalGraph -> Index -> TimeInterval -> Table (Maybe Time)
earliestArrivalTime g v (t1, t2)= 
    runSTArray (edgeStreamFoldr (trimByGreaterValue (getEdgeStream g) t2) earliestArrivalAux (getInitialState (bounds g) v t1))

latestDepartureAux :: FoldrStateFunc s
latestDepartureAux (v1, v2, (t, l)) set = 
    set >>= \m ->
        getValue m v1 >>= \t1 ->
            if maybeLTE (t + l) t1 then
                getValue m v2 >>= \t2 ->
                    if maybeGT t t2 then
                        setValue m v2 t >>= \_ -> return m
                    else
                        return m
            else
                return m

latestDepartureTime :: TemporalGraph -> Index -> TimeInterval -> Table (Maybe Time)
latestDepartureTime g v (t1, t2)= 
    runSTArray (edgeStreamFoldr (trimByLesserValue (getEdgeStream g) t1) latestDepartureAux (getInitialState (bounds g) v t2))