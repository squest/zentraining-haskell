fibos = 1:2: zipWith (+) fibos (tail fibos)

square = zipWith (*) [1..] [1..]

faktorial = scanl1 (*) [1..]

triangular = scanl1 (+) [1..]

--pentagonalBodoh = map (\x -> (round((x * (3 * x - 1)) / 2)) ) [1..]

pentagonal = scanl1 (+) [1,4..]

--powerOfbego x = zipWith (^) [x,x..] [1..]

--contoh dari peter
nNumbers = (!!) [(scanl1 (+) [1,i..]) | i <- [-1..]]

powerOf = (!!) [(zipWith (^) [i,i..] [1..]) | i <- [0..]]
