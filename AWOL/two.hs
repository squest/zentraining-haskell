--primes (infinite list of prime numbers, without using any function as described in the video)


--fibos => [1,2,3,5..]
--fibos x
--  | x == 1 = [1]
--  | x == 2 = [1, 2]
--  | otherwise



--primorials

--factorials
factorials x
  | x == 1 = 1
  | otherwise = x * factorials (pred x)

--triangular numbers n(n+1)/2




--pentagonal numbers n(3n - 1) / 2


--pascal => [[1],[1,1],[1,2,1]...]

--powerOf (infinite list of powerOf i)


--ex. powerOf 2 = [1,2,4,8,16,32...]

--sumPrimes => all possible summation of consecutive primes

--palindromes (starting from 1) => [1,2,3..11,22,33..101,111,121...]
