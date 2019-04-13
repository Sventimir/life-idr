module Life

import Data.Vect as V
import Data.SortedSet as S

public export
Cell : Type
Cell = (Int, Int)

public export
Life : Type
Life = SortedSet Cell

data NatRange : Type where
    Exclusive : (lower, upper : Nat) -> {auto ok : LTE lower upper} -> NatRange


export
mapCell : (Int -> Int) -> Cell -> Cell
mapCell f (x, y) = (f x, f y)

export
mapLeft : (Int -> Int) -> Cell -> Cell
mapLeft f (x, y) = (f x, y)

export
mapRight : (Int -> Int) -> Cell -> Cell
mapRight f (x, y) = (x, f y)

vectToList : Vect n a -> List a
vectToList = foldl (flip (::)) []

inRange : NatRange -> Nat -> Bool
inRange (Exclusive l u) n = l < n && n < u

populateRange : NatRange
populateRange = Exclusive 2 4

survivalRange : NatRange
survivalRange = Exclusive 1 4

vects : Vect 8 (Int, Int)
vects = [(-1, 1) , (0, 1) , (1, 1),
         (-1, 0) ,          (1, 0),
         (-1, -1), (0, -1), (1, -1)]

translate : Cell -> (Int, Int) -> Cell
translate (x, y) (i, j) = (x + i, y + j)

neighbours : Cell -> Vect 8 Cell
neighbours c = map (translate c) vects

freeNeighbours : Life -> Life
freeNeighbours s =
        flip difference s . fromList .
        concatMap (vectToList . neighbours) $
        S.toList s

aliveNeighbours : Life -> Cell -> Nat
aliveNeighbours s = Pairs.DPair.fst . filter (flip contains s) . neighbours

withAliveNeighbours : Life -> Cell -> (Cell, Nat)
withAliveNeighbours s c = (c, aliveNeighbours s c)

export
stepForward : Life -> Life
stepForward s = union
        (computeSet survivalRange s)
        (computeSet populateRange $ freeNeighbours s)
    where
    computeSet : NatRange -> Life -> Life
    computeSet r = S.fromList .
                map fst . filter (inRange r . snd) .
                map (withAliveNeighbours s) . S.toList

export
initial : Life
initial = empty
