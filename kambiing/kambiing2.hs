fibos = 1:1: zipWith (+) fibos (tail fibos)

square = zipWith (*) [1..] [1..]

fak = scanl1 (*) [1..]

triangular = scanl1 (+) [1..]

--pentagonalBodoh = map (\x -> (round((x * (3 * x - 1)) / 2)) ) [1..]

pentagonal = scanl1 (+) [1,4..]

--powerOfbego x = zipWith (^) [x,x..] [1..]

--contoh dari peter
nNumbers = (!!) [(scanl1 (+) [1,i..]) | i <- [-1..]]

powerOf = (!!) [(zipWith (^) [i,i..] [1..]) | i <- [0..]]

underPrime n = 2:sieveX [3,5..n]

--primes doodling
sieveX [] = []
sieveX (x:xs)
  | x < round(sqrt $ fromIntegral(last xs)) = x : sieveX (filter (\n -> (0 /= rem n x)) xs)
  | otherwise = x : xs

sieveOdd n = all (\x -> (0 /= rem n x)) $ takeWhile (\x -> x*x <= n ) [3,5..]


prime2 = 2:filter sieveOdd [3,5..]
prima = 2:sieveX [3,5..1000000]

isPrime n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = iter 3
  where iter i
          | isqr > n = True
          | 0 == rem n i = False
          | otherwise = iter $ i + 2
          where isqr = i*i
---
prime = 2:filter isPrime [3,5..]

primorial = scanl1 (*) $ prime

sumPrime = scanl1 (+) $ prime

--pascal ribet
--lazy faktorials for the binom
binomialCoefPasc n =round $ fak !! (head n) / ((fak !! (last n)) * (fak !! ((head n) - (last n))))

--nysun binom nya
rangeFrom1 y = [1..y]
susunPasc _ [] = []
susunPasc x (y:ys) = [x,y] : susunPasc x ys
susunPascUse x y = susunPasc x (rangeFrom1 y)

--
pascalTriangle 0 = [1]
pascalTriangle x = 1:(reverse $ 1 : map binomialCoefPasc (susunPascUse x (pred x)))

pascal = map pascalTriangle [0..]

--pascal beneran [data list]

pascalDat = iterate (\x -> [1] ++ zipWith (+) x (tail x) ++ [1]) [1]

--fibos = 1:1: zipWith (+) fibos (tail fibos)
