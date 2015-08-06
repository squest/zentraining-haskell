-- ISPRIME KW ====>>>>> (Si babi masih ga jalan)
belIsPrime n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = (any (\a -> ((rem n a) == 0)) [3..(sqrt n)])

 -- mending bikin oddPrime aja karena lebih ngaruh

 oddPrime n = iter 3
    where iter i
            | i > div n i = True
            | 0 == rem n i = False
            | otherwise = iter $ i+2

-- kalo mau versi elo, bisa gini, tapi ini sebenernya buggy, karena 3 pasti ga masuk

isPrime :: Integral a => a -> Bool
isPrime n
    | n < 2 = False
    | n == 2 = True
    | even n = False
    | otherwise = all (\x -> (rem n x) /= 0) [3..(round (sqrt n))]

-- another one, function yg dimasukin ke takeWhile tuh a trick untuk ngecheck whether dia seukuran sama sqrt n

isPrime' n
  | n < 2 = False
  | n == 2 = True
  | even n = False
  | otherwise = all (\x -> (rem n x) /= 0) $ takeWhile (\x -> x <= div n x) [3,5..]
