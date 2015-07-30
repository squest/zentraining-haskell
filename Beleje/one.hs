module One where

import Data.List

-- NGETES JALAN
squa x = x*x

-- NULL KW
belNull [] = True
belNull (x:xs) = False
-- belNull _ = False (dari Peeta)

-- TAKE KW
belTake i [] = []
belTake 0 (x:xs) = []
belTake i (x:xs) = [x] ++ (belTake (pred i) xs)

-- DROP KW
belDrop i [] = []
belDrop 0 (x:xs) = (x:xs)
belDrop i (x:xs) = (belDrop (pred i) xs)

-- FST KW
belFst (a, b) = a

-- SND KW
belSnd (a, b) = b
