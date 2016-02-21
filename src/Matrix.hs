module Matrix (
    Matrix,
    rowsNum,
    colsNum,
    toList,
    vectorH,
    vectorV,
    matrix,
    mult,
    kron,
    multPow,
    kronPow
) where

import Data.List (transpose)
import Utils

data Matrix a = Matrix {
    rowsNum :: Int,
    colsNum :: Int,
    matrixData :: [[a]]
}

instance Show a => Show (Matrix a) where
    show (Matrix _ _ xs) = unlines . map ((\s -> "(" ++ s ++ ")") .unwords . map show) $ xs

instance Functor Matrix where
    fmap f (Matrix r c xs) = Matrix r c (map (map f) xs)

matrix :: Num a => [[a]] -> Matrix a
matrix xs
    | correct = Matrix rows cols xs
    | otherwise = error "Incorrect matrix dimensions"
    where cs = map length xs
          cols = head cs
          rows = length xs
          correct = all (== cols) cs

vectorH :: Num a => [a] -> Matrix a
vectorH xs = Matrix 1 (length xs) [xs]

vectorV :: Num a => [a] -> Matrix a
vectorV xs = Matrix (length xs) 1 (map (: []) xs)

toList :: Matrix a -> [[a]]
toList = matrixData

mult :: Num a => Matrix a -> Matrix a -> Matrix a
mult (Matrix r1 c1 xs) (Matrix r2 c2 ys)
    | c1 /= r2 = error "Matrix dimensions disagree"
    | otherwise = Matrix r1 c2 ms
    where ys' = transpose ys
          ms = [[multv x y | y <- ys'] | x <- xs]
          multv x y = sum $ zipWith (*) x y

kron :: Num a => Matrix a -> Matrix a -> Matrix a
kron (Matrix r1 c1 xs) (Matrix r2 c2 ys) = Matrix r c ms
    where r = r1 * r2
          c = c1 * c2
          ms = concat [[ concat [[x * y | y <- yrow] | x <- xrow] | yrow <- ys ] | xrow <- xs]

multPow :: Num a => Int -> Matrix a -> Matrix a
multPow n xm@(Matrix r c _)
    | r /= c = error "Matrix must be square"
    | n < 1 = error "Negative or zero exponent"
    | otherwise = fpow2 n kron xm

kronPow :: Num a => Int -> Matrix a -> Matrix a
kronPow n xm
    | n < 1 = error "Negative or zero exponent"
    | otherwise = fpow2 n kron xm
