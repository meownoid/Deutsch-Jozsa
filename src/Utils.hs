module Utils where

import Data.Char (isSpace)

fpow :: Int -> (a -> a) -> a -> a
fpow n f x = iterate f x !! n

fpow2 :: Int -> (a -> a -> a) -> a ->  a
fpow2 n f xm = g n xm
    where g 1 ym = ym
          g n ym = g (n - 1) (ym `f` xm)

dec2bin :: Int -> Int -> [Int]
dec2bin n x = replicate (n - m) 0 ++ reverse xb
    where m = length xb
          xb = f (fromIntegral x)
          f 0  = []
          f x = (x `mod` 2) : f (x `div` 2)

xori :: Int -> Int -> Int
xori 1 0 = 1
xori 0 1 = 1
xori _ _ = 0

noti :: Int -> Int
noti 0 = 1
noti _ = 0

ori :: Int -> Int -> Int
ori 0 0 = 0
ori _ _ = 1

andi :: Int -> Int -> Int
andi 1 1 = 1
andi _ _ = 0


splitBy :: Eq a => a -> [a] -> [[a]]
splitBy del = foldr f [[]]
    where f c l@(x:xs) | c == del = []:l
                       | otherwise = (c:x):xs

lstrip :: [Char] -> [Char]
lstrip = dropWhile isSpace