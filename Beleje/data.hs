module Data where

import Data.List
import One

-- TRIANGLES (by SQuest HAHAHAHAHAHA)
triangles = belScanl1 (+) [0..]

-- PENTAGONAL
pentagonals = 0:belScanl1 (+) [1,4..]

-- FACTORIAL LIST
factorials1 = belScanl1 (*) [1..]
factorials2 = belMap (\a -> (belFoldl1 (*) [1..a])) [1..]

-- FIBBO LIST
fibbos = 0:1:belZipWith (+) fibbos (tail fibbos)

-- POWER OF
powerOf = (\a -> (belScanl1 (*) (cycle [a])))
