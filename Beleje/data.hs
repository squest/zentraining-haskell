module Data where

import Data.List
import One

-- TRIANGLES (by SQuest HAHAHAHAHAHA)
triangles = belScanl1 (+) [0..]

-- PENTAGONAL

-- FACTORIAL LIST
factorials1 = belScanl1 (*) [1..]
factorials2 = belMap (\a -> (belFoldl1 (*) [1..a])) [1..]

--
