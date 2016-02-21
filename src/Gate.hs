module Gate (
    gateH,
    gateI,
    makeBinaryGate
) where

import Control.Monad (replicateM)
import Quantum
import Matrix
import Utils

gateCheck :: Int -> Int
gateCheck n
    | n < 1 = error "Negative or zero parameter"
    | otherwise = n

gateH :: Int -> QGate
gateH n = makeGate (fmap (/ 2 ** (fromIntegral m / 2)) h)
    where m = gateCheck n
          h = kronPow m h1
          h1 = matrix [[1, 1], [1, -1]] :: Matrix Double

gateI :: Int -> QGate
gateI n = makeGate i
    where m = gateCheck n
          i = kronPow m i1
          i1 = matrix [[1, 0], [0, 1]] :: Matrix Double

makeBinaryGate :: Int -> ([Int] -> Int) -> QGate
makeBinaryGate n f = makeGateC . matrix . map qregisterL $ us
    where us = concat [[x ++ [y `xori` f x] | y <- ys] | x <- xs]
          xs = replicateM n [0, 1]
          ys = [0, 1]
