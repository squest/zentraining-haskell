module Two where

import Data.List

--primes (infinite list of prime numbers, without using any function as described in the video) X
--fibos => [1,2,3,5..] X
--primorials X
--factorials X
--triangular numbers
--pentagonal numbers
--pascal => [[1],[1,1],[1,2,1]...]
--powerOf (infinite list of powerOf i)
--ex. powerOf 2 = [1,2,4,8,16,32...]
--sumPrimes => all possible summation of consecutive primes

primekah x
  | elem 0 (tes x) == False = True
  | otherwise = False
  where tes x = map (rem x) (coba x)
                where coba x = takeWhile (\y -> y < x) [3,5..]

prime = 2:filter (primekah) [3,5..]

fak x
  |x == 0 = 1
  |otherwise = x *(fak $ x - 1)

factorial = map (fak ) [0..]

primorial = 1:scanl1 (*) prime

fibos = 1:2:zipWith (+) fibos (tail fibos)
