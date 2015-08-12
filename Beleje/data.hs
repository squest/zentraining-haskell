module Data where

import Data.List
import One

-- TRIANGLES (by SQuest HAHAHAHAHAHA)
triangles = belScanl1 (+) [0..]

-- PENTAGONAL
pentagonals = 0:belScanl1 (+) [1,4..]

-- FACTORIAL LIST
factorials = belMap (\a -> (belFoldl1 (*) [1..a])) [1..]

-- FIBBO LIST
fibbos = 0:1:belZipWith (+) fibbos (tail fibbos)

-- POWER OF
powerOfa = (\a -> (belScanl (*) 1 (belCycle [a])))
powerOfb = (nth (belMap (\a -> belMap (a ^) [0,1..]) [0,1..]))

-- PASCAL
pascals = belIterate (\a -> belZipWith (+) (0:a) (belReverse (0:a))) [1]

-- PRIME (Ga bisa prime, babay)
primes = filter (\a -> all (\b -> (rem a b) /= 0) (takeWhile (\v -> v * v <= a) primes)) (iterate succ 2)

-- PRIMORIALS

-- SUMPRIME

-- PALINDROMES
pals = (belFilter (\a -> (show a) == reverse (show a)) [1..])
