module Data where

import Data.List
import One

-- TRIANGLES (by SQuest HAHAHAHAHAHA)
triangles = belScanl1 (+) [0..]

-- PENTAGONAL
pentagonals = belScanl1 (+) [1,4..]

--

fak' 0 = 1
fak' n = n * fak' (n-1)

fakList' [] = []
fakList' x = map fak' x
